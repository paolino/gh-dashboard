-- | Repo-related action handlers.
-- |
-- | This module handles all actions related to the
-- | Repos page:
-- |
-- | **Data fetching:**
-- | - `handleRefreshRepo`     — re-fetch a single repo
-- | - `handleRefreshIssues`   — fetch open issues
-- | - `handleRefreshIssue`    — refresh one issue
-- | - `handleRefreshPRs`      — fetch open PRs + checks
-- | - `handleRefreshPR`       — refresh one PR
-- | - `handleRefreshWorkflows`— fetch workflow runs
-- | - `handleWorkflowPrevSha` / `handleWorkflowNextSha`
-- |
-- | **UI interactions:**
-- | - `handleToggleExpand`    — expand/collapse a repo
-- | - `handleToggleItem`      — expand/collapse a
-- |   detail section (issues, PRs, workflows) and
-- |   auto-fetch on first open.
-- | - `handleDrag*`           — repo reordering
-- | - `handleSubmitAddRepo`   — add a repo by URL
-- | - `handleRemoveRepo`      — remove a repo
-- | - `handleHideItem`        — hide/unhide items
-- |
-- | Each handler receives a `Dispatch` callback for
-- | cross-module action calls (e.g. refreshing agent
-- | sessions after certain operations).
module Action.Repos
  ( handleRefreshRepo
  , handleRefreshIssues
  , handleRefreshIssue
  , handleRefreshPRs
  , handleRefreshPR
  , handleRefreshWorkflows
  , handleWorkflowPrevSha
  , handleWorkflowNextSha
  , handleToggleExpand
  , handleToggleItem
  , handleDragStart
  , handleDragDrop
  , handleSubmitAddRepo
  , handleRemoveRepo
  , handleHideItem
  , loadWorkflowShaDetails
  , extractShas
  ) where

import Prelude

import Action.Common
  ( Dispatch
  , HalogenAction
  , guardExpanded
  , persistView
  , termElementId
  , toggleSet
  , updateDetail
  )
import Data.Array
  ( filter
  , find
  , index
  , length
  , null
  , nubByEq
  , snoc
  , any
  )
import Data.Either (Either(..))
import Data.Foldable (traverse_)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Data.Tuple (Tuple(..))
import Effect.Class (liftEffect)
import FFI.Terminal (attachTerminal, destroyTerminal)
import GitHub.Rest
  ( fetchCheckRuns
  , fetchCommitPRs
  , fetchCommitStatuses
  , fetchRepo
  , fetchRepoIssues
  , fetchIssue
  , fetchRepoPRs
  , fetchWorkflowJobs
  , fetchWorkflowRuns
  )
import Halogen as H
import Refresh (refreshSinglePR)
import RepoUtils (moveItem, orderRepos, parseRepoName, upsertRepo)
import Storage (saveRepoList)
import Types
  ( Issue(..)
  , PullRequest(..)
  , Repo(..)
  , WorkflowJob(..)
  , WorkflowRun(..)
  )
import View.Types (Action(..), State)
import Effect.Aff (Aff)
import Web.HTML (window)
import Web.HTML.Window (confirm)

handleRefreshRepo
  :: forall o. String -> HalogenAction o
handleRefreshRepo fullName = do
  st <- H.get
  result <- H.liftAff
    (fetchRepo st.token fullName)
  case result of
    Left _ -> pure unit
    Right repo ->
      H.modify_ _
        { repos = upsertRepo repo st.repos }

handleRefreshIssues
  :: forall o. Dispatch o -> HalogenAction o
handleRefreshIssues _ = do
  st <- H.get
  case st.expanded of
    Nothing -> pure unit
    Just fullName -> do
      H.modify_ _ { issuesLoading = true }
      result <- H.liftAff
        (fetchRepoIssues st.token fullName)
      let
        issues = case result of
          Right is -> is
          Left _ -> []
      guardExpanded fullName $
        updateDetail \d -> d
          { issues = issues
          , issueCount = length issues
          }
      H.modify_ _ { issuesLoading = false }

handleRefreshIssue
  :: forall o. Int -> HalogenAction o
handleRefreshIssue issueNum = do
  st <- H.get
  case st.expanded of
    Nothing -> pure unit
    Just fullName -> do
      result <- H.liftAff
        (fetchIssue st.token fullName issueNum)
      case result of
        Left _ -> pure unit
        Right issue ->
          guardExpanded fullName $
            updateDetail \d -> d
              { issues = map
                  ( \(Issue i) ->
                      if i.number == issueNum then
                        issue
                      else Issue i
                  )
                  d.issues
              }

handleRefreshPRs
  :: forall o. Dispatch o -> HalogenAction o
handleRefreshPRs _ = do
  st <- H.get
  case st.expanded of
    Nothing -> pure unit
    Just fullName -> do
      H.modify_ _ { prsLoading = true }
      prsResult <- H.liftAff
        (fetchRepoPRs st.token fullName)
      let
        prs = case prsResult of
          Right ps -> ps
          Left _ -> []
      guardExpanded fullName $
        updateDetail \d -> d
          { pullRequests = []
          , prCount = length prs
          , prChecks = Map.empty
          }
      traverse_
        ( \pr@(PullRequest p) ->
            guardExpanded fullName do
              let
                isVisible = not
                  ( Set.member p.htmlUrl
                      st.hiddenItems
                  )
              checks <-
                if isVisible then do
                  st3 <- H.get
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
              updateDetail \d -> d
                { pullRequests =
                    snoc d.pullRequests pr
                , prChecks =
                    case checks of
                      Nothing -> d.prChecks
                      Just (Tuple n c) ->
                        Map.insert n c
                          d.prChecks
                }
        )
        prs
      H.modify_ _ { prsLoading = false }

handleRefreshPR
  :: forall o. Int -> HalogenAction o
handleRefreshPR prNum = do
  st <- H.get
  case st.expanded of
    Nothing -> pure unit
    Just fullName ->
      refreshSinglePR st.token fullName prNum

handleRefreshWorkflows
  :: forall o. Dispatch o -> HalogenAction o
handleRefreshWorkflows _ = do
  st <- H.get
  case st.expanded of
    Nothing -> pure unit
    Just fullName -> do
      let
        branch =
          case
            find
              (\(Repo r) -> r.fullName == fullName)
              st.repos
            of
            Just (Repo r) -> r.defaultBranch
            Nothing -> "main"
      H.modify_ _ { workflowsLoading = true }
      result <- H.liftAff
        ( fetchWorkflowRuns st.token fullName
            branch
        )
      let
        runs = case result of
          Right rs -> rs
          Left _ -> []
        shas = extractShas runs
        shaCount = length shas
      guardExpanded fullName do
        updateDetail \d -> d
          { workflowRuns = runs
          , workflowCount = shaCount
          , workflowJobs = Map.empty
          , workflowShaIndex = 0
          , workflowShaPRs = Map.empty
          }
        loadWorkflowShaDetails fullName
      H.modify_ _ { workflowsLoading = false }

handleWorkflowPrevSha
  :: forall o. HalogenAction o
handleWorkflowPrevSha = do
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

handleWorkflowNextSha
  :: forall o. HalogenAction o
handleWorkflowNextSha = do
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

handleToggleExpand
  :: forall o. String -> HalogenAction o
handleToggleExpand fullName = do
  st <- H.get
  if st.expanded == Just fullName then do
    H.modify_ _
      { expanded = Nothing
      }
    persistView
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
  persistView

handleToggleItem
  :: forall o
   . Dispatch o
  -> String
  -> HalogenAction o
handleToggleItem dispatch key = do
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
  -- When collapsing a section with an active
  -- terminal, detach it.
  when (not opening) do
    case Map.lookup key st.terminalKeys of
      Nothing -> pure unit
      Just itemKey -> do
        liftEffect $ destroyTerminal
          (termElementId itemKey)
        H.modify_ \s -> s
          { launchedItems = Set.delete
              itemKey
              s.launchedItems
          , terminalKeys = Map.delete key
              s.terminalKeys
          , terminalUrls = Map.delete
              itemKey
              s.terminalUrls
          }
  persistView
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
        dispatch RefreshIssues
      "section-prs" ->
        dispatch RefreshPRs
      "section-workflows" ->
        dispatch RefreshWorkflows
      _ -> pure unit

handleDragStart
  :: forall o. String -> HalogenAction o
handleDragStart fullName =
  H.modify_ _ { dragging = Just fullName }

handleDragDrop
  :: forall o. String -> HalogenAction o
handleDragDrop targetName = do
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

handleSubmitAddRepo
  :: forall o. HalogenAction o
handleSubmitAddRepo = do
  st <- H.get
  case parseRepoName st.addRepoInput of
    Nothing ->
      H.modify_ _
        { error = Just "Enter a GitHub URL" }
    Just name -> do
      let
        alreadyExists = any
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

handleRemoveRepo
  :: forall o. String -> HalogenAction o
handleRemoveRepo fullName = do
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

handleHideItem
  :: forall o. String -> HalogenAction o
handleHideItem url = do
  st <- H.get
  H.modify_ _
    { hiddenItems = toggleSet url st.hiddenItems }
  persistView

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

-- | Load jobs and PR info for the currently
-- | selected SHA.
loadWorkflowShaDetails
  :: forall o. String -> HalogenAction o
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
              case prResult of
                Right (Just pr) ->
                  updateDetail \d -> d
                    { workflowShaPRs =
                        Map.insert sha pr
                          d.workflowShaPRs
                    }
                _ -> pure unit
          -- Fetch jobs for each run
          traverse_
            ( \(WorkflowRun wr) ->
                guardExpanded fullName do
                  st3 <- H.get
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
                  when (not (null nonSuccess)) $
                    updateDetail \d -> d
                      { workflowJobs =
                          Map.insert
                            wr.name
                            nonSuccess
                            d.workflowJobs
                      }
            )
            shaRuns

