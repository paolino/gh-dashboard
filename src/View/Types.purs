-- | View types — Action and State shared across view modules.
module View.Types
  ( Action(..)
  , State
  ) where

import Data.Maybe (Maybe)
import Data.Set (Set)
import GitHub (RateLimit)
import Types (Repo, RepoDetail)

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

-- | Application state (referenced by view).
type State =
  { token :: String
  , repos :: Array Repo
  , expanded :: Maybe String
  , details :: Maybe RepoDetail
  , detailLoading :: Boolean
  , loading :: Boolean
  , error :: Maybe String
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
  }
