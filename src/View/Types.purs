-- | View types — Action and State shared across view modules.
module View.Types
  ( Action(..)
  , State
  ) where

import Data.Maybe (Maybe)
import Data.Set (Set)
import GitHub (RateLimit)
import Data.Map (Map)
import Types
  ( Page
  , Project
  , ProjectItem
  , Repo
  , RepoDetail
  , StatusField
  )

-- | Actions emitted by the view.
data Action
  = Initialize
  | SetToken String
  | SubmitToken
  | RefreshRepo String
  | RefreshIssues
  | RefreshIssue Int
  | RefreshPRs
  | RefreshPR Int
  | ToggleExpand String
  | ToggleItem String
  | SetFilter String
  | DragStart String
  | DragDrop String
  | ToggleAddRepo
  | SetAddRepoInput String
  | SubmitAddRepo
  | RemoveRepo String
  | HideItem String
  | CopyText String
  | ToggleIssueLabelFilter String
  | TogglePRLabelFilter String
  | RefreshWorkflows
  | WorkflowPrevSha
  | WorkflowNextSha
  | ToggleWorkflowStatusFilter String
  | ToggleTheme
  | ExportStorage
  | ImportStorage
  | ResetAll
  | SwitchPage Page
  | RefreshProjects
  | ExpandProject String
  | RefreshProjectItems String
  | RefreshProjectItem String String Int
  | ToggleProjectRepoFilter String
  | SetItemStatus String String String
  | SetNewItemTitle String
  | SubmitNewItem String
  | StartEditItem String String
  | SetEditItemTitle String
  | SubmitEditItem String String String
  | DeleteItem String String
  | StartRenameProject String String
  | SetRenameProjectTitle String
  | SubmitRenameProject String String
  | LaunchAgent String String Int
  | DetachAgent String Int
  | StopAgent String Int
  | SetAgentServer String
  | RefreshAgentSessions
  | ToggleSessionFilter String

-- | Application state (referenced by view).
type State =
  { token :: String
  , repos :: Array Repo
  , expanded :: Maybe String
  , details :: Maybe RepoDetail
  , detailLoading :: Boolean
  , loading :: Boolean
  , error :: Maybe String
  , info :: Maybe String
  , rateLimit :: Maybe RateLimit
  , filterText :: String
  , hasToken :: Boolean
  , expandedItems :: Set String
  , repoList :: Array String
  , hiddenItems :: Set String
  , dragging :: Maybe String
  , showAddRepo :: Boolean
  , addRepoInput :: String
  , darkTheme :: Boolean
  , issuesLoading :: Boolean
  , prsLoading :: Boolean
  , workflowsLoading :: Boolean
  , issueLabelFilters :: Set String
  , prLabelFilters :: Set String
  , workflowStatusFilters :: Set String
  , currentPage :: Page
  , projects :: Array Project
  , projectsLoading :: Boolean
  , expandedProject :: Maybe String
  , projectItems :: Map String (Array ProjectItem)
  , projectItemsLoading :: Boolean
  , projectRepoFilters :: Set String
  , projectStatusFields :: Map String StatusField
  , newItemTitle :: String
  , editingItem :: Maybe String
  , editItemTitle :: String
  , editingProject :: Maybe String
  , editProjectTitle :: String
  , agentServer :: String
  , launchedItems :: Set String
  , terminalKeys :: Map String String
  , terminalUrls :: Map String String
  , agentSessions :: Map String String
  , agentWorktrees :: Set String
  , sessionFilters :: Set String
  }
