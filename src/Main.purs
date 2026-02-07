module Main where

import Prelude

import Data.Array (filter, findIndex, length, null, take)
import Data.Array as Array
import Data.Traversable (traverse, traverse_)
import Data.Either (Either(..))
import Data.Map as Map
import Data.Tuple (Tuple(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set as Set
import Data.String (Pattern(..), contains, toLower, trim)
import Data.String (replaceAll, split) as Str
import Data.String.Pattern (Replacement(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import GitHub
  ( fetchCheckRuns
  , fetchCommitStatuses
  , fetchIssue
  , fetchPR
  , fetchRepo
  , fetchRepoIssues
  , fetchRepoPRs
  , fetchUserRepos
  )
import Halogen as H
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Storage
  ( clearAll
  , loadHidden
  , loadRepoList
  , loadToken
  , saveRepoList
  , saveToken
  , saveHidden
  )
import Types (Issue(..), PullRequest(..), Repo(..))
import View (Action(..), State, renderDashboard, renderTokenForm)
import Web.HTML (window)
import Web.HTML.Window (confirm)

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI rootComponent unit body

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
  , filterText: ""
  , hasToken: false
  , expandedItems: Set.empty
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
        }
      doRefresh st.token
  RefreshRepo fullName -> do
    st <- H.get
    result <- H.liftAff
      (fetchRepo st.token fullName)
    case result of
      Left _ -> pure unit
      Right repo ->
        H.modify_ _
          { repos = upsertRepo repo st.repos }
  RefreshIssues -> do
    st <- H.get
    case st.expanded of
      Nothing -> pure unit
      Just fullName -> do
        result <- H.liftAff
          (fetchRepoIssues st.token fullName)
        st2 <- H.get
        when (st2.expanded == Just fullName) do
          let
            issues = case result of
              Right is -> is
              Left _ -> []
          case st2.details of
            Nothing ->
              H.modify_ _
                { details = Just
                    { issues
                    , pullRequests: []
                    , issueCount: length issues
                    , prCount: 0
                    , prChecks: Map.empty
                    }
                }
            Just detail ->
              H.modify_ _
                { details = Just detail
                    { issues = issues
                    , issueCount = length issues
                    }
                }
  RefreshIssue issueNum -> do
    st <- H.get
    case st.expanded of
      Nothing -> pure unit
      Just fullName -> do
        result <- H.liftAff
          (fetchIssue st.token fullName issueNum)
        st2 <- H.get
        when (st2.expanded == Just fullName) do
          case result of
            Left _ -> pure unit
            Right issue ->
              case st2.details of
                Nothing ->
                  H.modify_ _
                    { details = Just
                        { issues: [ issue ]
                        , pullRequests: []
                        , issueCount: 1
                        , prCount: 0
                        , prChecks: Map.empty
                        }
                    }
                Just detail ->
                  let
                    updated = map
                      ( \(Issue i) ->
                          if i.number == issueNum
                            then issue
                          else Issue i
                      )
                      detail.issues
                  in
                    H.modify_ _
                      { details = Just detail
                          { issues = updated }
                      }
  RefreshPRs -> do
    st <- H.get
    case st.expanded of
      Nothing -> pure unit
      Just fullName -> do
        prsResult <- H.liftAff
          (fetchRepoPRs st.token fullName)
        st2 <- H.get
        when (st2.expanded == Just fullName) do
          let
            prs = case prsResult of
              Right ps -> ps
              Left _ -> []
            visiblePRs = filter
              ( \(PullRequest p) ->
                  not
                    ( Set.member p.htmlUrl
                        st2.hiddenItems
                    )
              )
              prs
          checkResults <- H.liftAff $ traverse
            ( \(PullRequest pr) -> do
                cr <- fetchCheckRuns st2.token
                  fullName
                  pr.headSha
                cs <- fetchCommitStatuses
                  st2.token
                  fullName
                  pr.headSha
                let
                  runs = case cr of
                    Right r -> r
                    Left _ -> []
                  statuses = case cs of
                    Right s -> s
                    Left _ -> []
                pure $ Tuple pr.number
                  (runs <> statuses)
            )
            visiblePRs
          let
            checks = Map.fromFoldable checkResults
          case st2.details of
            Nothing ->
              H.modify_ _
                { details = Just
                    { issues: []
                    , pullRequests: prs
                    , issueCount: 0
                    , prCount: length prs
                    , prChecks: checks
                    }
                }
            Just detail ->
              H.modify_ _
                { details = Just detail
                    { pullRequests = prs
                    , prCount = length prs
                    , prChecks = checks
                    }
                }
  RefreshPR prNum -> do
    st <- H.get
    case st.expanded of
      Nothing -> pure unit
      Just fullName ->
        refreshSinglePR st.token fullName prNum
  ToggleExpand fullName -> do
    st <- H.get
    if st.expanded == Just fullName then
      H.modify_ _
        { expanded = Nothing
        , details = Nothing
        , detailLoading = false
        , expandedItems = Set.empty
        }
    else
      H.modify_ _
        { expanded = Just fullName
        , detailLoading = false
        , details = Nothing
        , expandedItems = Set.empty
        }
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
    ok <- liftEffect do
      w <- window
      confirm ("Remove " <> fullName <> "?") w
    when ok do
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
          seeded = take 25 repos
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
    traverse_
      ( \name -> do
          result <- H.liftAff (fetchRepo token name)
          case result of
            Left _ -> pure unit
            Right repo -> do
              st2 <- H.get
              let
                updated = upsertRepo repo st2.repos
              H.modify_ _
                { repos = orderRepos st2.repoList
                    updated
                }
      )
      st.repoList
    H.modify_ _ { loading = false }

-- | Insert or update a repo in the array.
upsertRepo :: Repo -> Array Repo -> Array Repo
upsertRepo repo@(Repo r) repos =
  if Array.any
    (\(Repo x) -> x.fullName == r.fullName)
    repos
  then
    map
      ( \(Repo x) ->
          if x.fullName == r.fullName then repo
          else Repo x
      )
      repos
  else Array.snoc repos repo

-- | Order repos to match the stored list.
orderRepos :: Array String -> Array Repo -> Array Repo
orderRepos order repos = Array.catMaybes $ map
  ( \name -> Array.find
      (\(Repo r) -> r.fullName == name)
      repos
  )
  order

-- | Re-fetch a single PR and its check runs.
refreshSinglePR
  :: forall o
   . String
  -> String
  -> Int
  -> H.HalogenM State Action () o Aff Unit
refreshSinglePR token fullName prNum = do
  prResult <- H.liftAff
    (fetchPR token fullName prNum)
  case prResult of
    Left _ -> pure unit
    Right newPR@(PullRequest pr) -> do
      cr <- H.liftAff $
        fetchCheckRuns token fullName pr.headSha
      cs <- H.liftAff $
        fetchCommitStatuses token fullName
          pr.headSha
      let
        runs = case cr of
          Right r -> r
          Left _ -> []
        statuses = case cs of
          Right s -> s
          Left _ -> []
        newChecks = runs <> statuses
      st <- H.get
      case st.details of
        Nothing ->
          H.modify_ _
            { details = Just
                { issues: []
                , pullRequests: [ newPR ]
                , issueCount: 0
                , prCount: 1
                , prChecks: Map.singleton prNum
                    newChecks
                }
            }
        Just detail ->
          let
            updated = map
              ( \(PullRequest p) ->
                  if p.number == prNum then newPR
                  else PullRequest p
              )
              detail.pullRequests
          in
            H.modify_ _
              { details = Just detail
                  { pullRequests = updated
                  , prChecks = Map.insert prNum
                      newChecks
                      detail.prChecks
                  }
              }

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
