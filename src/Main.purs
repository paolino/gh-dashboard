-- | Application entry point and action dispatcher.
-- |
-- | This module is intentionally thin. It wires
-- | together:
-- |
-- | 1. **Halogen component** — initialState, render,
-- |    and the eval spec.
-- | 2. **Action dispatch** — routes each `Action` to
-- |    the appropriate handler module:
-- |    - `Action.Repos`    — repo/issue/PR/workflow
-- |    - `Action.Projects` — project board CRUD
-- |    - `Action.Agent`    — terminal/session mgmt
-- |    - Inline handlers   — trivial one-liners
-- |      (SetToken, ToggleTheme, etc.)
-- | 3. **Initialization** — loads persisted state from
-- |    localStorage, restores view, and kicks off
-- |    the first data fetch.
-- |
-- | If you're looking for the actual handler logic,
-- | see the `Action.*` modules. If you're looking for
-- | the view layer, see `View` and `View.*`.
module Main where

import Prelude

import Action.Agent
  ( handleDetachAgent
  , handleLaunchAgent
  , handleRefreshAgentSessions
  , handleSetAgentServer
  , handleStopAgent
  , handleToggleSessionFilter
  )
import Action.Common
  ( persistView
  , toggleSet
  )
import Action.Projects
  ( handleDeleteItem
  , handleExpandProject
  , handleRefreshProjectItem
  , handleRefreshProjectItems
  , handleRefreshProjects
  , handleSetEditItemTitle
  , handleSetItemStatus
  , handleSetNewItemTitle
  , handleSetRenameProjectTitle
  , handleStartEditItem
  , handleStartRenameProject
  , handleSubmitEditItem
  , handleSubmitNewItem
  , handleSubmitRenameProject
  , handleToggleProjectRepoFilter
  )
import Action.Repos
  ( handleDragDrop
  , handleDragStart
  , handleHideItem
  , handleRefreshIssue
  , handleRefreshIssues
  , handleRefreshPR
  , handleRefreshPRs
  , handleRefreshRepo
  , handleRefreshWorkflows
  , handleRemoveRepo
  , handleSubmitAddRepo
  , handleToggleExpand
  , handleToggleItem
  , handleWorkflowNextSha
  , handleWorkflowPrevSha
  )
import Data.Array (null)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import FFI.Cache as FFI.Cache
import FFI.Clipboard (copyToClipboard)
import FFI.Storage as FFIStorage
import FFI.Theme (setBodyTheme)
import Halogen as H
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Refresh (doRefresh)
import RepoUtils (applyFilter)
import Storage
  ( clearAll
  , loadAgentServer
  , loadRepoList
  , loadToken
  , loadViewState
  , saveToken
  )
import Types (Page(..))
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

-- | Initial application state with all fields
-- | set to their empty/default values. Persisted
-- | state is loaded in the Initialize handler.
initialState :: State
initialState =
  { token: ""
  , repos: []
  , expanded: Nothing
  , details: Nothing
  , detailLoading: false
  , loading: false
  , error: Nothing
  , info: Nothing
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
  , currentPage: ReposPage
  , projects: []
  , projectsLoading: false
  , expandedProject: Nothing
  , projectItems: Map.empty
  , projectItemsLoading: false
  , projectRepoFilters: Set.empty
  , projectStatusFields: Map.empty
  , newItemTitle: ""
  , editingItem: Nothing
  , editItemTitle: ""
  , editingProject: Nothing
  , editProjectTitle: ""
  , agentServer: ""
  , launchedItems: Set.empty
  , terminalKeys: Map.empty
  , terminalUrls: Map.empty
  , agentSessions: Map.empty
  , agentWorktrees: Set.empty
  , sessionFilters: Set.empty
  }

render :: forall m. State -> H.ComponentHTML Action () m
render state =
  if state.hasToken then
    renderDashboard state
      (applyFilter state.filterText state.repos)
  else
    renderTokenForm state

------------------------------------------------------------
-- Action dispatcher
------------------------------------------------------------

-- | Route each action to its handler module.
-- |
-- | Cross-module calls (e.g. a project handler
-- | needing to refresh agent sessions) go through
-- | `handleAction` itself, passed as a `Dispatch`
-- | callback.
handleAction
  :: forall o
   . Action
  -> H.HalogenM State Action () o Aff Unit
handleAction = case _ of

  ------------------------------------------------
  -- Initialization & auth
  ------------------------------------------------

  Initialize -> do
    saved <- liftEffect loadToken
    repoList <- liftEffect loadRepoList
    agentUrl <- liftEffect loadAgentServer
    vs <- liftEffect loadViewState
    liftEffect $ setBodyTheme vs.darkTheme
    H.modify_ _
      { repoList = repoList
      , agentServer = agentUrl
      , hiddenItems = vs.hiddenItems
      , darkTheme = vs.darkTheme
      , issueLabelFilters = vs.issueLabelFilters
      , prLabelFilters = vs.prLabelFilters
      , workflowStatusFilters =
          vs.workflowStatusFilters
      , currentPage = vs.currentPage
      , expanded = vs.expanded
      , expandedProject = vs.expandedProject
      , expandedItems = vs.expandedItems
      , filterText = vs.filterText
      , projectRepoFilters = vs.projectRepoFilters
      }
    case saved of
      "" -> pure unit
      tok -> do
        H.modify_ _
          { token = tok, hasToken = true }
        handleAction RefreshAgentSessions
        case vs.currentPage of
          ReposPage -> doRefresh tok
          ProjectsPage -> do
            handleAction RefreshProjects
            case vs.expandedProject of
              Nothing -> pure unit
              Just projId ->
                handleAction
                  (RefreshProjectItems projId)

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

  ------------------------------------------------
  -- Repo actions (delegated)
  ------------------------------------------------

  RefreshRepo fullName ->
    handleRefreshRepo fullName
  RefreshIssues ->
    handleRefreshIssues handleAction
  RefreshIssue n ->
    handleRefreshIssue n
  RefreshPRs ->
    handleRefreshPRs handleAction
  RefreshPR n ->
    handleRefreshPR n
  RefreshWorkflows ->
    handleRefreshWorkflows handleAction
  WorkflowPrevSha ->
    handleWorkflowPrevSha
  WorkflowNextSha ->
    handleWorkflowNextSha
  ToggleExpand fullName ->
    handleToggleExpand fullName
  ToggleItem key ->
    handleToggleItem handleAction key
  DragStart fullName ->
    handleDragStart fullName
  DragDrop targetName ->
    handleDragDrop targetName
  SubmitAddRepo ->
    handleSubmitAddRepo
  RemoveRepo fullName ->
    handleRemoveRepo fullName
  HideItem url ->
    handleHideItem url

  ------------------------------------------------
  -- Inline one-liners (not worth a module)
  ------------------------------------------------

  SetFilter txt -> do
    H.modify_ _ { filterText = txt }
    persistView

  ToggleAddRepo -> do
    st <- H.get
    H.modify_ _
      { showAddRepo = not st.showAddRepo
      , addRepoInput = ""
      }

  SetAddRepoInput txt ->
    H.modify_ _ { addRepoInput = txt }

  CopyText text ->
    liftEffect $ copyToClipboard text

  ToggleIssueLabelFilter label -> do
    st <- H.get
    H.modify_ _
      { issueLabelFilters =
          toggleSet label st.issueLabelFilters
      }
    persistView

  TogglePRLabelFilter label -> do
    st <- H.get
    H.modify_ _
      { prLabelFilters =
          toggleSet label st.prLabelFilters
      }
    persistView

  ToggleWorkflowStatusFilter status -> do
    st <- H.get
    H.modify_ _
      { workflowStatusFilters =
          toggleSet status st.workflowStatusFilters
      }
    persistView

  ToggleTheme -> do
    st <- H.get
    let dark = not st.darkTheme
    H.modify_ _ { darkTheme = dark }
    liftEffect $ setBodyTheme dark
    persistView

  ExportStorage ->
    liftEffect FFIStorage.exportStorage

  ImportStorage ->
    liftEffect FFIStorage.importStorage

  ResetAll -> do
    ok <- liftEffect do
      w <- window
      confirm "Reset all saved data?" w
    when ok do
      H.liftAff FFI.Cache.clearCache
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
        , info = Nothing
        , loading = false
        , darkTheme = true
        , projects = []
        , currentPage = ReposPage
        , expandedProject = Nothing
        , projectItems = Map.empty
        }

  SwitchPage page -> do
    H.modify_ _ { currentPage = page }
    persistView
    handleAction RefreshAgentSessions
    st <- H.get
    case page of
      ProjectsPage ->
        when (null st.projects) do
          handleAction RefreshProjects
      ReposPage ->
        when (null st.repos) do
          doRefresh st.token

  ------------------------------------------------
  -- Project actions (delegated)
  ------------------------------------------------

  RefreshProjects ->
    handleRefreshProjects handleAction
  ExpandProject projectId ->
    handleExpandProject handleAction projectId
  RefreshProjectItems projectId ->
    handleRefreshProjectItems
      handleAction projectId
  RefreshProjectItem pid repo num ->
    handleRefreshProjectItem pid repo num
  ToggleProjectRepoFilter repo ->
    handleToggleProjectRepoFilter repo
  SetItemStatus pid iid status ->
    handleSetItemStatus pid iid status
  SetNewItemTitle t ->
    handleSetNewItemTitle t
  SubmitNewItem projectId ->
    handleSubmitNewItem handleAction projectId
  StartEditItem itemId title ->
    handleStartEditItem itemId title
  SetEditItemTitle t ->
    handleSetEditItemTitle t
  SubmitEditItem pid did title ->
    handleSubmitEditItem pid did title
  DeleteItem pid iid ->
    handleDeleteItem pid iid
  StartRenameProject pid title ->
    handleStartRenameProject pid title
  SetRenameProjectTitle t ->
    handleSetRenameProjectTitle t
  SubmitRenameProject pid title ->
    handleSubmitRenameProject pid title

  ------------------------------------------------
  -- Agent actions (delegated)
  ------------------------------------------------

  LaunchAgent toggleKey fullName issueNum ->
    handleLaunchAgent handleAction
      toggleKey fullName issueNum
  DetachAgent fullName issueNum ->
    handleDetachAgent fullName issueNum
  StopAgent fullName issueNum ->
    handleStopAgent handleAction
      fullName issueNum
  SetAgentServer url ->
    handleSetAgentServer url
  RefreshAgentSessions ->
    handleRefreshAgentSessions
  ToggleSessionFilter label ->
    handleToggleSessionFilter label
