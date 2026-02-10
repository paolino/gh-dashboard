module Main where

import Prelude

import Data.Array (filter, index, length, nubByEq, null)
import Data.Array as Array
import Data.Traversable (traverse_)
import Data.Either (Either(..))
import Data.Map as Map
import Data.Tuple (Tuple(..))
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import GitHub
  ( fetchCheckRuns
  , fetchCommitPRs
  , fetchCommitStatuses
  , fetchIssue
  , fetchRepo
  , fetchRepoIssues
  , fetchRepoPRs
  , fetchWorkflowJobs
  , fetchWorkflowRuns
  )
import Halogen as H
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import FFI.Clipboard (copyToClipboard)
import FFI.Storage as FFIStorage
import FFI.Theme (setBodyTheme)
import Refresh (doRefresh, refreshSinglePR)
import RepoUtils
  ( applyFilter
  , moveItem
  , orderRepos
  , parseRepoName
  , upsertRepo
  )
import Storage
  ( clearAll
  , loadHidden
  , loadIssueLabelFilters
  , loadPRLabelFilters
  , loadRepoList
  , loadTheme
  , loadToken
  , saveIssueLabelFilters
  , savePRLabelFilters
  , saveRepoList
  , saveTheme
  , saveToken
  , saveHidden
  )
import Types
  ( Issue(..)
  , PullRequest(..)
  , Repo(..)
  , WorkflowJob(..)
  , WorkflowRun(..)
  )
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
  , darkTheme: true
  , issuesLoading: false
  , prsLoading: false
  , workflowsLoading: false
  , issueLabelFilters: Set.empty
  , prLabelFilters: Set.empty
  , workflowStatusFilters: Set.empty
  }

render :: forall m. State -> H.ComponentHTML Action () m
render state =
  if state.hasToken then
    renderDashboard state
      (applyFilter state.filterText state.repos)
  else
    renderTokenForm state

handleAction
  :: forall o
   . Action
  -> H.HalogenM State Action () o Aff Unit
handleAction = case _ of
  Initialize -> do
    saved <- liftEffect loadToken
    repoList <- liftEffect loadRepoList
    hidden <- liftEffect loadHidden
    dark <- liftEffect loadTheme
    issueLabels <- liftEffect loadIssueLabelFilters
    prLabels <- liftEffect loadPRLabelFilters
    liftEffect $ setBodyTheme dark
    H.modify_ _
      { repoList = repoList
      , hiddenItems = hidden
      , darkTheme = dark
      , issueLabelFilters = issueLabels
      , prLabelFilters = prLabels
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
        H.modify_ _ { issuesLoading = true }
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
                    , workflowRuns: []
                    , workflowCount: 0
                    , workflowJobs: Map.empty
                    , workflowShaIndex: 0
                    , workflowShaPRs: Map.empty
                    }
                }
            Just detail ->
              H.modify_ _
                { details = Just detail
                    { issues = issues
                    , issueCount = length issues
                    }
                }
        H.modify_ _ { issuesLoading = false }
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
                        , workflowRuns: []
                        , workflowCount: 0
                        , workflowJobs: Map.empty
                        , workflowShaIndex: 0
                        , workflowShaPRs: Map.empty
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
        H.modify_ _ { prsLoading = true }
        prsResult <- H.liftAff
          (fetchRepoPRs st.token fullName)
        st2 <- H.get
        when (st2.expanded == Just fullName) do
          let
            prs = case prsResult of
              Right ps -> ps
              Left _ -> []
          case st2.details of
            Nothing ->
              H.modify_ _
                { details = Just
                    { issues: []
                    , pullRequests: []
                    , issueCount: 0
                    , prCount: length prs
                    , prChecks: Map.empty
                    , workflowRuns: []
                    , workflowCount: 0
                    , workflowJobs: Map.empty
                    , workflowShaIndex: 0
                    , workflowShaPRs: Map.empty
                    }
                }
            Just detail ->
              H.modify_ _
                { details = Just detail
                    { pullRequests = []
                    , prCount = length prs
                    , prChecks = Map.empty
                    }
                }
          traverse_
            ( \pr@(PullRequest p) -> do
                st3 <- H.get
                when
                  (st3.expanded == Just fullName)
                  do
                    let
                      isVisible = not
                        ( Set.member p.htmlUrl
                            st3.hiddenItems
                        )
                    checks <-
                      if isVisible then do
                        cr <- H.liftAff $
                          fetchCheckRuns st3.token
                            fullName
                            p.headSha
                        cs <- H.liftAff $
                          fetchCommitStatuses
                            st3.token
                            fullName
                            p.headSha
                        let
                          runs = case cr of
                            Right r -> r
                            Left _ -> []
                          statuses = case cs of
                            Right s -> s
                            Left _ -> []
                        pure $ Just $ Tuple
                          p.number
                          (runs <> statuses)
                      else pure Nothing
                    st4 <- H.get
                    case st4.details of
                      Nothing -> pure unit
                      Just detail ->
                        H.modify_ _
                          { details = Just
                              detail
                                { pullRequests =
                                    Array.snoc
                                      detail.pullRequests
                                      pr
                                , prChecks =
                                    case checks of
                                      Nothing ->
                                        detail.prChecks
                                      Just
                                        ( Tuple n
                                            c
                                        ) ->
                                        Map.insert n
                                          c
                                          detail.prChecks
                                }
                          }
            )
            prs
        H.modify_ _ { prsLoading = false }
  RefreshPR prNum -> do
    st <- H.get
    case st.expanded of
      Nothing -> pure unit
      Just fullName ->
        refreshSinglePR st.token fullName prNum
  RefreshWorkflows -> do
    st <- H.get
    case st.expanded of
      Nothing -> pure unit
      Just fullName -> do
        let
          branch = "master"
        H.modify_ _ { workflowsLoading = true }
        result <- H.liftAff
          ( fetchWorkflowRuns st.token fullName
              branch
          )
        st2 <- H.get
        when (st2.expanded == Just fullName) do
          let
            runs = case result of
              Right rs -> rs
              Left _ -> []
            shas = extractShas runs
            shaCount = length shas
          case st2.details of
            Nothing ->
              H.modify_ _
                { details = Just
                    { issues: []
                    , pullRequests: []
                    , issueCount: 0
                    , prCount: 0
                    , prChecks: Map.empty
                    , workflowRuns: runs
                    , workflowCount: shaCount
                    , workflowJobs: Map.empty
                    , workflowShaIndex: 0
                    , workflowShaPRs: Map.empty
                    }
                }
            Just detail ->
              H.modify_ _
                { details = Just detail
                    { workflowRuns = runs
                    , workflowCount = shaCount
                    , workflowJobs = Map.empty
                    , workflowShaIndex = 0
                    , workflowShaPRs = Map.empty
                    }
                }
          loadWorkflowShaDetails fullName
        H.modify_ _ { workflowsLoading = false }
  WorkflowPrevSha -> do
    st <- H.get
    case st.details of
      Nothing -> pure unit
      Just detail -> do
        let
          idx = detail.workflowShaIndex
        when (idx > 0) do
          H.modify_ _
            { details = Just detail
                { workflowShaIndex = idx - 1
                , workflowJobs = Map.empty
                }
            , workflowStatusFilters = Set.empty
            }
          case st.expanded of
            Nothing -> pure unit
            Just fullName ->
              loadWorkflowShaDetails fullName
  WorkflowNextSha -> do
    st <- H.get
    case st.details of
      Nothing -> pure unit
      Just detail -> do
        let
          idx = detail.workflowShaIndex
          shas = extractShas detail.workflowRuns
          maxIdx = length shas - 1
        when (idx < maxIdx) do
          H.modify_ _
            { details = Just detail
                { workflowShaIndex = idx + 1
                , workflowJobs = Map.empty
                }
            , workflowStatusFilters = Set.empty
            }
          case st.expanded of
            Nothing -> pure unit
            Just fullName ->
              loadWorkflowShaDetails fullName
  ToggleExpand fullName -> do
    st <- H.get
    if st.expanded == Just fullName then
      H.modify_ _
        { expanded = Nothing
        }
    else do
      let
        switching = st.expanded /= Nothing
          && st.expanded /= Just fullName
      H.modify_ _
        { expanded = Just fullName
        , detailLoading = false
        , details =
            if switching then Nothing
            else st.details
        , expandedItems =
            if switching then Set.empty
            else st.expandedItems
        }
  ToggleItem key -> do
    st <- H.get
    let
      opening = not
        (Set.member key st.expandedItems)
    H.modify_ _
      { expandedItems =
          if opening then
            Set.insert key st.expandedItems
          else
            Set.delete key st.expandedItems
      }
    when opening do
      let
        empty = case st.details of
          Nothing -> true
          Just d
            | key == "section-issues" ->
                null d.issues
            | key == "section-prs" ->
                null d.pullRequests
            | key == "section-workflows" ->
                null d.workflowRuns
            | otherwise -> false
      when empty case key of
        "section-issues" ->
          handleAction RefreshIssues
        "section-prs" ->
          handleAction RefreshPRs
        "section-workflows" ->
          handleAction RefreshWorkflows
        _ -> pure unit
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
  CopyText text ->
    liftEffect $ copyToClipboard text
  ToggleIssueLabelFilter label -> do
    st <- H.get
    let
      newFilters =
        if Set.member label
          st.issueLabelFilters
        then
          Set.delete label
            st.issueLabelFilters
        else
          Set.insert label
            st.issueLabelFilters
    H.modify_ _ { issueLabelFilters = newFilters }
    liftEffect $ saveIssueLabelFilters newFilters
  TogglePRLabelFilter label -> do
    st <- H.get
    let
      newFilters =
        if Set.member label st.prLabelFilters
        then
          Set.delete label st.prLabelFilters
        else
          Set.insert label st.prLabelFilters
    H.modify_ _ { prLabelFilters = newFilters }
    liftEffect $ savePRLabelFilters newFilters
  ToggleWorkflowStatusFilter status -> do
    st <- H.get
    let
      newFilters =
        if Set.member status
          st.workflowStatusFilters
        then
          Set.delete status
            st.workflowStatusFilters
        else
          Set.insert status
            st.workflowStatusFilters
    H.modify_ _
      { workflowStatusFilters = newFilters }
  ToggleTheme -> do
    st <- H.get
    let dark = not st.darkTheme
    H.modify_ _ { darkTheme = dark }
    liftEffect do
      saveTheme dark
      setBodyTheme dark
  ExportStorage ->
    liftEffect FFIStorage.exportStorage
  ImportStorage ->
    liftEffect FFIStorage.importStorage
  ResetAll -> do
    ok <- liftEffect do
      w <- window
      confirm "Reset all saved data?" w
    when ok do
      liftEffect do
        clearAll
        setBodyTheme true
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
        , darkTheme = true
        }

-- | Extract unique SHAs from runs, preserving order.
extractShas :: Array WorkflowRun -> Array String
extractShas runs =
  map (\(WorkflowRun r) -> r.headSha)
    ( nubByEq
        ( \(WorkflowRun a) (WorkflowRun b) ->
            a.headSha == b.headSha
        )
        runs
    )

-- | Load jobs and PR info for the currently selected SHA.
loadWorkflowShaDetails
  :: forall o
   . String
  -> H.HalogenM State Action () o Aff Unit
loadWorkflowShaDetails fullName = do
  st <- H.get
  case st.details of
    Nothing -> pure unit
    Just detail -> do
      let
        shas = extractShas detail.workflowRuns
        idx = detail.workflowShaIndex
      case index shas idx of
        Nothing -> pure unit
        Just sha -> do
          let
            shaRuns = nubByEq
              ( \(WorkflowRun a) (WorkflowRun b) ->
                  a.name == b.name
              )
              ( filter
                  ( \(WorkflowRun r) ->
                      r.headSha == sha
                  )
                  detail.workflowRuns
              )
          -- Fetch PR for this SHA if not cached
          when
            ( not
                ( Map.member sha
                    detail.workflowShaPRs
                )
            )
            do
              prResult <- H.liftAff $
                fetchCommitPRs st.token fullName sha
              st2 <- H.get
              case prResult of
                Right (Just pr) ->
                  case st2.details of
                    Nothing -> pure unit
                    Just d ->
                      H.modify_ _
                        { details = Just d
                            { workflowShaPRs =
                                Map.insert sha pr
                                  d.workflowShaPRs
                            }
                        }
                _ -> pure unit
          -- Fetch jobs for non-success runs
          traverse_
            ( \(WorkflowRun wr) -> do
                st3 <- H.get
                when
                  (st3.expanded == Just fullName)
                  do
                    jobsResult <- H.liftAff $
                      fetchWorkflowJobs st3.token
                        fullName
                        wr.runId
                    let
                      nonSuccess =
                        case jobsResult of
                          Right js -> filter
                            ( \(WorkflowJob j) ->
                                j.conclusion
                                  /= Just "success"
                            )
                            js
                          Left _ -> []
                    when (not (null nonSuccess)) do
                      st4 <- H.get
                      case st4.details of
                        Nothing -> pure unit
                        Just d ->
                          H.modify_ _
                            { details = Just d
                                { workflowJobs =
                                    Map.insert
                                      wr.name
                                      nonSuccess
                                      d.workflowJobs
                                }
                            }
            )
            shaRuns
