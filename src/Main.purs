module Main where

import Prelude

import Data.Array (filter, length, sortBy)
import Data.Either (Either(..))
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Ordering (invert)
import Data.String (Pattern(..), contains, toLower)
import Effect (Effect)
import Effect.Aff (Aff, Milliseconds(..), delay)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import GitHub
  ( fetchRepoIssues
  , fetchRepoPRs
  , fetchUserRepos
  )
import Halogen as H
import Halogen.Aff as HA
import Halogen.Subscription as HS
import Halogen.VDom.Driver (runUI)
import Types (Repo(..), RepoDetail)
import View
  ( Action(..)
  , SortDir(..)
  , SortField(..)
  , State
  , renderDashboard
  , renderTokenForm
  )
import Web.HTML (window)
import Web.HTML.Window (confirm, localStorage)
import Web.Storage.Storage as Storage

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI rootComponent unit body

storageKeyToken :: String
storageKeyToken = "gh-dashboard-token"

rootComponent
  :: forall q i o. H.Component q i o Aff
rootComponent =
  H.mkComponent
    { initialState: \_ -> initialState
    , render
    , eval: H.mkEval H.defaultEval
        { handleAction = handleAction
        , initialize = Just Initialize
        }
    }

initialState :: State
initialState =
  { token: ""
  , repos: []
  , expanded: Nothing
  , details: Nothing
  , detailLoading: false
  , loading: false
  , error: Nothing
  , rateLimit: Nothing
  , interval: 60
  , secondsLeft: 60
  , sortField: SortUpdated
  , sortDir: Desc
  , filterText: ""
  , hasToken: false
  }

render :: forall m. State -> H.ComponentHTML Action () m
render state =
  if state.hasToken then
    renderDashboard state (applySort state (applyFilter state state.repos))
  else
    renderTokenForm state

-- | Filter repos by name/description.
applyFilter :: State -> Array Repo -> Array Repo
applyFilter state repos
  | state.filterText == "" = repos
  | otherwise =
      let
        q = toLower state.filterText
      in
        filter
          ( \(Repo r) ->
              contains (Pattern q) (toLower r.name)
                || contains (Pattern q)
                  (toLower (fromMaybe "" r.description))
          )
          repos

-- | Sort repos by selected field.
applySort :: State -> Array Repo -> Array Repo
applySort state = sortBy comparator
  where
  dir = case state.sortDir of
    Asc -> identity
    Desc -> invert
  comparator (Repo a) (Repo b) = dir $ case state.sortField of
    SortName -> compare
      (toLower a.name)
      (toLower b.name)
    SortUpdated -> compare a.updatedAt b.updatedAt
    SortIssues -> compare
      a.openIssuesCount
      b.openIssuesCount

type InternalState =
  { tickSub :: Maybe H.SubscriptionId
  , detailCache :: Map.Map String RepoDetail
  }

handleAction
  :: forall o
   . Action
  -> H.HalogenM State Action () o Aff Unit
handleAction = case _ of
  Initialize -> do
    saved <- liftEffect loadToken
    case saved of
      "" -> pure unit
      tok -> do
        H.modify_ _
          { token = tok, hasToken = true }
        doRefresh tok
    _ <- H.subscribe $ ticker 1000.0
    pure unit
  Tick -> do
    st <- H.get
    if not st.hasToken then pure unit
    else if st.secondsLeft <= 1 then do
      H.modify_ _
        { loading = true
        , secondsLeft = st.interval
        }
      doRefresh st.token
    else
      H.modify_ _
        { secondsLeft = st.secondsLeft - 1 }
  SetToken tok ->
    H.modify_ _ { token = tok }
  SubmitToken -> do
    st <- H.get
    if st.token == "" then
      H.modify_ _
        { error = Just "Please enter a token" }
    else do
      liftEffect $ saveToken st.token
      H.modify_ _
        { hasToken = true
        , error = Nothing
        , loading = true
        , secondsLeft = st.interval
        }
      doRefresh st.token
  Refresh -> do
    st <- H.get
    H.modify_ _
      { loading = true
      , secondsLeft = st.interval
      }
    doRefresh st.token
  ToggleExpand fullName -> do
    st <- H.get
    if st.expanded == Just fullName then
      H.modify_ _
        { expanded = Nothing
        , details = Nothing
        , detailLoading = false
        }
    else do
      H.modify_ _
        { expanded = Just fullName
        , detailLoading = true
        , details = Nothing
        }
      fetchDetail st.token fullName
  SetSort field -> do
    st <- H.get
    if st.sortField == field then
      H.modify_ _ { sortDir = flipDir st.sortDir }
    else
      H.modify_ _
        { sortField = field
        , sortDir = Desc
        }
  SetFilter txt ->
    H.modify_ _ { filterText = txt }
  ChangeInterval delta -> do
    st <- H.get
    let
      newInterval = max 10 (st.interval + delta)
    H.modify_ _
      { interval = newInterval
      , secondsLeft = min st.secondsLeft
          newInterval
      }
  ResetAll -> do
    ok <- liftEffect do
      w <- window
      confirm "Reset all saved data?" w
    when ok do
      liftEffect clearToken
      H.modify_ _ { token = "", hasToken = false, repos = [], expanded = Nothing, details = Nothing, error = Nothing, loading = false }

flipDir :: SortDir -> SortDir
flipDir Asc = Desc
flipDir Desc = Asc

-- | Fetch all repos.
doRefresh
  :: forall o
   . String
  -> H.HalogenM State Action () o Aff Unit
doRefresh token = do
  result <- H.liftAff (fetchUserRepos token)
  case result of
    Left err ->
      H.modify_ _
        { error = Just err, loading = false }
    Right { repos, rateLimit } ->
      H.modify_ _
        { repos = repos
        , rateLimit = rateLimit
        , loading = false
        , error = Nothing
        }

-- | Fetch detail for one repo.
fetchDetail
  :: forall o
   . String
  -> String
  -> H.HalogenM State Action () o Aff Unit
fetchDetail token fullName = do
  issuesResult <- H.liftAff
    (fetchRepoIssues token fullName)
  prsResult <- H.liftAff
    (fetchRepoPRs token fullName)
  st <- H.get
  when (st.expanded == Just fullName) do
    let
      issues = case issuesResult of
        Right is -> is
        Left _ -> []
      prs = case prsResult of
        Right ps -> ps
        Left _ -> []
      detail =
        { issues
        , pullRequests: prs
        , issueCount: length issues
        , prCount: length prs
        }
    H.modify_ _
      { details = Just detail
      , detailLoading = false
      }

ticker :: Number -> HS.Emitter Action
ticker ms = HS.makeEmitter \emit -> do
  fiber <- Aff.launchAff do
    loop emit
  pure $ Aff.launchAff_
    (Aff.killFiber (Aff.error "unsubscribe") fiber)
  where
  loop emit = do
    delay (Milliseconds ms)
    liftEffect (emit Tick)
    loop emit

-- localStorage helpers

loadToken :: Effect String
loadToken = do
  w <- window
  s <- localStorage w
  fromMaybe "" <$> Storage.getItem storageKeyToken s

saveToken :: String -> Effect Unit
saveToken tok = do
  w <- window
  s <- localStorage w
  Storage.setItem storageKeyToken tok s

clearToken :: Effect Unit
clearToken = do
  w <- window
  s <- localStorage w
  Storage.removeItem storageKeyToken s
