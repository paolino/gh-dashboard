module Main where

import Prelude

import Data.Array (filter, findIndex, length, null, take)
import Data.Array as Array
import Data.Traversable (traverse)
import Data.Either (Either(..))
import Data.Map as Map
import Data.Tuple (Tuple(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set as Set
import Data.String (Pattern(..), contains, toLower, trim)
import Data.String (split) as Str
import Data.String.Pattern (Replacement(..))
import Data.String (replaceAll) as Str
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
  ( RateLimit
  , fetchCommitStatus
  , fetchRepo
  , fetchRepoIssues
  , fetchRepoPRs
  , fetchUserRepos
  )
import Halogen as H
import Halogen.Aff as HA
import Halogen.Subscription as HS
import Halogen.VDom.Driver (runUI)
import Types (PullRequest(..), Repo(..))
import View (Action(..), State, renderDashboard, renderTokenForm)
import Web.HTML (window)
import Web.HTML.Window (confirm, localStorage)
import Web.Storage.Storage as Storage

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI rootComponent unit body

storageKeyToken :: String
storageKeyToken = "gh-dashboard-token"

storageKeyRepos :: String
storageKeyRepos = "gh-dashboard-repos"

storageKeyHidden :: String
storageKeyHidden = "gh-dashboard-hidden"

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
  , filterText: ""
  , hasToken: false
  , expandedItems: Set.empty
  , autoRefresh: true
  , repoList: []
  , hiddenItems: Set.empty
  , dragging: Nothing
  , showAddRepo: false
  , addRepoInput: ""
  }

render :: forall m. State -> H.ComponentHTML Action () m
render state =
  if state.hasToken then
    renderDashboard state (applyFilter state state.repos)
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

handleAction
  :: forall o
   . Action
  -> H.HalogenM State Action () o Aff Unit
handleAction = case _ of
  Initialize -> do
    saved <- liftEffect loadToken
    repoList <- liftEffect loadRepoList
    hidden <- liftEffect loadHidden
    H.modify_ _
      { repoList = repoList
      , hiddenItems = hidden
      }
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
  DragStart fullName ->
    H.modify_ _ { dragging = Just fullName }
  DragDrop targetName -> do
    st <- H.get
    case st.dragging of
      Nothing -> pure unit
      Just srcName -> do
        H.modify_ _ { dragging = Nothing }
        when (srcName /= targetName) do
          let
            newList = moveItem srcName targetName
              st.repoList
            newRepos = orderRepos newList st.repos
          H.modify_ _
            { repoList = newList
            , repos = newRepos
            }
          liftEffect $ saveRepoList newList
  ToggleAddRepo -> do
    st <- H.get
    H.modify_ _
      { showAddRepo = not st.showAddRepo
      , addRepoInput = ""
      }
  SetAddRepoInput txt ->
    H.modify_ _ { addRepoInput = txt }
  SubmitAddRepo -> do
    st <- H.get
    case parseRepoName st.addRepoInput of
      Nothing ->
        H.modify_ _
          { error = Just "Enter a GitHub URL" }
      Just name -> do
        let
          alreadyExists = Array.any
            (\(Repo r) -> r.fullName == name)
            st.repos
        if alreadyExists then
          H.modify_ _
            { error = Just
                (name <> " is already in the list")
            , showAddRepo = false
            , addRepoInput = ""
            }
        else do
          result <- H.liftAff
            (fetchRepo st.token name)
          case result of
            Left err ->
              H.modify_ _
                { error = Just err }
            Right repo -> do
              st2 <- H.get
              let
                newList = [ name ] <> st2.repoList
              H.modify_ _
                { repos = [ repo ] <> st2.repos
                , repoList = newList
                , showAddRepo = false
                , addRepoInput = ""
                , error = Nothing
                }
              liftEffect $ saveRepoList newList
  RemoveRepo fullName -> do
    st <- H.get
    let
      newList = filter (_ /= fullName) st.repoList
      newRepos = filter
        (\(Repo r) -> r.fullName /= fullName)
        st.repos
    H.modify_ _
      { repoList = newList
      , repos = newRepos
      , expanded =
          if st.expanded == Just fullName then
            Nothing
          else st.expanded
      , details =
          if st.expanded == Just fullName then
            Nothing
          else st.details
      }
    liftEffect $ saveRepoList newList
  HideItem url -> do
    st <- H.get
    let
      newHidden =
        if Set.member url st.hiddenItems then
          Set.delete url st.hiddenItems
        else Set.insert url st.hiddenItems
    H.modify_ _ { hiddenItems = newHidden }
    liftEffect $ saveHidden newHidden
  ResetAll -> do
    ok <- liftEffect do
      w <- window
      confirm "Reset all saved data?" w
    when ok do
      liftEffect clearAll
      H.modify_ _
        { token = ""
        , hasToken = false
        , repos = []
        , repoList = []
        , hiddenItems = Set.empty
        , expanded = Nothing
        , details = Nothing
        , error = Nothing
        , loading = false
        }

-- | Extract owner/repo from a GitHub URL or plain name.
parseRepoName :: String -> Maybe String
parseRepoName input =
  let
    stripped = Str.replaceAll
      (Pattern "https://github.com/")
      (Replacement "")
      ( Str.replaceAll
          (Pattern "http://github.com/")
          (Replacement "")
          (trim input)
      )
    parts = filter (_ /= "")
      (Str.split (Pattern "/") stripped)
  in
    case parts of
      [ owner, repo ] -> Just (owner <> "/" <> repo)
      _ -> Nothing

-- | Fetch repos. If repoList is empty, seed from API.
doRefresh
  :: forall o
   . String
  -> H.HalogenM State Action () o Aff Unit
doRefresh token = do
  st <- H.get
  if null st.repoList then do
    result <- H.liftAff (fetchUserRepos token)
    case result of
      Left err ->
        H.modify_ _
          { error = Just err, loading = false }
      Right { repos, rateLimit } -> do
        let
          seeded = take 15 repos
          names = map
            (\(Repo r) -> r.fullName)
            seeded
        H.modify_ _
          { repos = seeded
          , repoList = names
          , rateLimit = rateLimit
          , loading = false
          , error = Nothing
          }
        liftEffect $ saveRepoList names
  else do
    results <- H.liftAff $
      fetchRepoList token st.repoList
    H.modify_ _
      { repos = orderRepos st.repoList results.repos
      , rateLimit = results.rateLimit
      , loading = false
      , error = results.error
      }

-- | Fetch all repos in the stored list individually.
fetchRepoList
  :: String
  -> Array String
  -> Aff
       { repos :: Array Repo
       , rateLimit :: Maybe RateLimit
       , error :: Maybe String
       }
fetchRepoList token names = do
  results <- traverse (fetchRepo token) names
  let
    repos = Array.catMaybes $ map
      ( case _ of
          Right r -> Just r
          Left _ -> Nothing
      )
      results
    firstErr = Array.findMap
      ( case _ of
          Left e -> Just e
          Right _ -> Nothing
      )
      results
  pure
    { repos
    , rateLimit: Nothing
    , error: firstErr
    }

-- | Order repos to match the stored list.
orderRepos :: Array String -> Array Repo -> Array Repo
orderRepos order repos = Array.catMaybes $ map
  ( \name -> Array.find
      (\(Repo r) -> r.fullName == name)
      repos
  )
  order

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
    statusResults <- H.liftAff $ traverse
      ( \(PullRequest pr) -> do
          res <- fetchCommitStatus token fullName
            pr.headSha
          pure $ Tuple pr.number
            ( case res of
                Right s -> s
                Left _ -> "unknown"
            )
      )
      prs
    let
      statuses = Map.fromFoldable statusResults
      detail =
        { issues
        , pullRequests: prs
        , issueCount: length issues
        , prCount: length prs
        , prStatuses: statuses
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

loadRepoList :: Effect (Array String)
loadRepoList = do
  w <- window
  s <- localStorage w
  raw <- Storage.getItem storageKeyRepos s
  pure $ case raw of
    Nothing -> []
    Just str ->
      case
        jsonParser str
          >>= (lmap printJsonDecodeError <<< decodeJson)
        of
        Right arr -> arr
        Left _ -> []

saveRepoList :: Array String -> Effect Unit
saveRepoList repos = do
  w <- window
  s <- localStorage w
  Storage.setItem storageKeyRepos
    (stringify (encodeJson repos))
    s

clearAll :: Effect Unit
clearAll = do
  w <- window
  s <- localStorage w
  Storage.removeItem storageKeyToken s
  Storage.removeItem storageKeyRepos s
  Storage.removeItem storageKeyHidden s

loadHidden :: Effect (Set.Set String)
loadHidden = do
  w <- window
  s <- localStorage w
  raw <- Storage.getItem storageKeyHidden s
  pure $ case raw of
    Nothing -> Set.empty
    Just str ->
      case
        jsonParser str
          >>= (lmap printJsonDecodeError <<< decodeJson)
        of
        Right (arr :: Array String) ->
          Set.fromFoldable arr
        Left _ -> Set.empty

saveHidden :: Set.Set String -> Effect Unit
saveHidden hidden = do
  w <- window
  s <- localStorage w
  let
    arr :: Array String
    arr = Set.toUnfoldable hidden
  Storage.setItem storageKeyHidden
    (stringify (encodeJson arr))
    s

moveItem :: String -> String -> Array String -> Array String
moveItem src target order =
  let
    without = filter (_ /= src) order
  in
    case findIndex (_ == target) without of
      Nothing -> order
      Just idx ->
        fromMaybe order
          (Array.insertAt idx src without)
