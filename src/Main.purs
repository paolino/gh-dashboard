module Main where

import Prelude

import Data.Array (filter, findIndex, length, null, sortBy)
import Data.Array as Array
import Data.Traversable (traverse)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set as Set
import Data.Ordering (invert)
import Data.String (Pattern(..), contains, split, toLower, trim)
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
  ( fetchRepo
  , fetchRepoIssues
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
  , dragging: Nothing
  , showAddRepo: false
  , addRepoInput: ""
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
  comparator (Repo a) (Repo b) = case state.sortField of
    SortCustom -> compare
      (orderIndex state.customOrder a.fullName)
      (orderIndex state.customOrder b.fullName)
    other -> dir $ case other of
      SortName -> compare
        (toLower a.name)
        (toLower b.name)
      SortUpdated -> compare a.updatedAt b.updatedAt
      SortIssues -> compare
        a.openIssuesCount
        b.openIssuesCount
      SortCustom -> EQ

handleAction
  :: forall o
   . Action
  -> H.HalogenM State Action () o Aff Unit
handleAction = case _ of
  Initialize -> do
    saved <- liftEffect loadToken
    order <- liftEffect loadOrder
    sortField <- liftEffect loadSort
    H.modify_ _ { customOrder = order, sortField = sortField }
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
    else do
      when (field == SortCustom) do
        when (null st.customOrder) do
          let
            currentOrder = map
              (\(Repo r) -> r.fullName)
              ( applySort st
                  (applyFilter st st.repos)
              )
          H.modify_ _
            { customOrder = currentOrder }
          liftEffect $ saveOrder currentOrder
      liftEffect $ saveSort field
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
            order = ensureOrder st
            newOrder = moveItem srcName targetName
              order
          H.modify_ _ { customOrder = newOrder }
          liftEffect $ saveOrder newOrder
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
              pinned <- liftEffect loadPinned
              let
                newPinned = pinned <> [ name ]
              liftEffect $ savePinned newPinned
              st2 <- H.get
              let
                newOrder = [ name ]
                  <> st2.customOrder
              H.modify_ _
                { repos = [ repo ] <> st2.repos
                , customOrder = newOrder
                , showAddRepo = false
                , addRepoInput = ""
                , error = Nothing
                }
              liftEffect $ saveOrder newOrder
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
      (split (Pattern "/") stripped)
  in
    case parts of
      [ owner, repo ] -> Just (owner <> "/" <> repo)
      _ -> Nothing

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
    Right { repos, rateLimit } -> do
      pinned <- liftEffect loadPinned
      let
        ownedNames = map
          (\(Repo r) -> r.fullName)
          repos
        extraNames = filter
          (\n -> not (Array.elem n ownedNames))
          pinned
      extras <- H.liftAff $ fetchPinned token
        extraNames
      H.modify_ _
        { repos = repos <> extras
        , rateLimit = rateLimit
        , loading = false
        , error = Nothing
        }

-- | Fetch pinned repos individually.
fetchPinned
  :: String
  -> Array String
  -> Aff (Array Repo)
fetchPinned token names = do
  results <- traverse (\n -> fetchRepo token n) names
  pure $ Array.catMaybes $ map
    ( case _ of
        Right r -> Just r
        Left _ -> Nothing
    )
    results

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

storageKeySort :: String
storageKeySort = "gh-dashboard-sort"

loadSort :: Effect SortField
loadSort = do
  w <- window
  s <- localStorage w
  raw <- Storage.getItem storageKeySort s
  pure $ case raw of
    Just "name" -> SortName
    Just "updated" -> SortUpdated
    Just "issues" -> SortIssues
    Just "custom" -> SortCustom
    _ -> SortUpdated

saveSort :: SortField -> Effect Unit
saveSort field = do
  w <- window
  s <- localStorage w
  Storage.setItem storageKeySort
    ( case field of
        SortName -> "name"
        SortUpdated -> "updated"
        SortIssues -> "issues"
        SortCustom -> "custom"
    )
    s

storageKeyPinned :: String
storageKeyPinned = "gh-dashboard-pinned"

loadPinned :: Effect (Array String)
loadPinned = do
  w <- window
  s <- localStorage w
  raw <- Storage.getItem storageKeyPinned s
  pure $ case raw of
    Nothing -> []
    Just str -> case jsonParser str >>= (lmap printJsonDecodeError <<< decodeJson) of
      Right arr -> arr
      Left _ -> []

savePinned :: Array String -> Effect Unit
savePinned pinned = do
  w <- window
  s <- localStorage w
  Storage.setItem storageKeyPinned
    (stringify (encodeJson pinned))
    s

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
