module Main where

import Prelude

import Data.Array (filter, findIndex, length, sortBy, (!!))
import Data.Array as Array
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set as Set
import Data.Ordering (invert)
import Data.String (Pattern(..), contains, toLower)
import Data.Argonaut.Core (stringify)
import Data.Argonaut.Decode.Class (decodeJson)
import Data.Argonaut.Decode.Error (printJsonDecodeError)
import Data.Argonaut.Encode.Class (encodeJson)
import Data.Argonaut.Parser (jsonParser)
import Data.Bifunctor (lmap)
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
import Types (Repo(..))
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
  , expandedItems: Set.empty
  , autoRefresh: true
  , customOrder: []
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
    SortCustom -> compare
      (orderIndex state.customOrder a.fullName)
      (orderIndex state.customOrder b.fullName)

handleAction
  :: forall o
   . Action
  -> H.HalogenM State Action () o Aff Unit
handleAction = case _ of
  Initialize -> do
    saved <- liftEffect loadToken
    order <- liftEffect loadOrder
    H.modify_ _ { customOrder = order }
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
    if not st.hasToken || not st.autoRefresh then
      pure unit
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
        , expandedItems = Set.empty
        }
    else do
      H.modify_ _
        { expanded = Just fullName
        , detailLoading = true
        , details = Nothing
        , expandedItems = Set.empty
        }
      fetchDetail st.token fullName
  ToggleItem key -> do
    st <- H.get
    H.modify_ _
      { expandedItems =
          if Set.member key st.expandedItems then
            Set.delete key st.expandedItems
          else Set.insert key st.expandedItems
      }
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
  ToggleAutoRefresh -> do
    st <- H.get
    H.modify_ _
      { autoRefresh = not st.autoRefresh
      , secondsLeft = st.interval
      }
  ChangeInterval delta -> do
    st <- H.get
    let
      newInterval = max 10 (st.interval + delta)
    H.modify_ _
      { interval = newInterval
      , secondsLeft = min st.secondsLeft
          newInterval
      }
  MoveUp fullName -> do
    st <- H.get
    let
      order = ensureOrder st
    case findIndex (_ == fullName) order of
      Just idx | idx > 0 -> do
        let newOrder = swapAt (idx - 1) idx order
        H.modify_ _ { customOrder = newOrder }
        liftEffect $ saveOrder newOrder
      _ -> pure unit
  MoveDown fullName -> do
    st <- H.get
    let
      order = ensureOrder st
    case findIndex (_ == fullName) order of
      Just idx
        | idx < Array.length order - 1 -> do
            let
              newOrder = swapAt idx (idx + 1) order
            H.modify_ _
              { customOrder = newOrder }
            liftEffect $ saveOrder newOrder
      _ -> pure unit
  ResetAll -> do
    ok <- liftEffect do
      w <- window
      confirm "Reset all saved data?" w
    when ok do
      liftEffect clearToken
      H.modify_ _ { token = "", hasToken = false, repos = [], expanded = Nothing, details = Nothing, error = Nothing, loading = false }

-- | Ensure custom order contains all current repos.
ensureOrder :: State -> Array String
ensureOrder st =
  let
    repoNames = map (\(Repo r) -> r.fullName) st.repos
    existing = filter
      (\n -> Array.elem n repoNames)
      st.customOrder
    missing = filter
      (\n -> not (Array.elem n existing))
      repoNames
  in
    existing <> missing

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

storageKeyOrder :: String
storageKeyOrder = "gh-dashboard-order"

loadOrder :: Effect (Array String)
loadOrder = do
  w <- window
  s <- localStorage w
  raw <- Storage.getItem storageKeyOrder s
  pure $ case raw of
    Nothing -> []
    Just str -> case jsonParser str >>= (lmap printJsonDecodeError <<< decodeJson) of
      Right arr -> arr
      Left _ -> []

saveOrder :: Array String -> Effect Unit
saveOrder order = do
  w <- window
  s <- localStorage w
  Storage.setItem storageKeyOrder
    (stringify (encodeJson order))
    s

orderIndex :: Array String -> String -> Int
orderIndex order name =
  fromMaybe 999999 (findIndex (_ == name) order)

swapAt :: Int -> Int -> Array String -> Array String
swapAt i j arr = case arr !! i, arr !! j of
  Just a, Just b ->
    fromMaybe arr
      ( Array.updateAt i b arr
          >>= Array.updateAt j a
      )
  _, _ -> arr
