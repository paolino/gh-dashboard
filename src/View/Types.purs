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
  | Tick
  | SetToken String
  | SubmitToken
  | Refresh
  | ToggleExpand String
  | ToggleItem String
  | SetFilter String
  | ToggleAutoRefresh
  | ChangeInterval Int
  | DragStart String
  | DragDrop String
  | ToggleAddRepo
  | SetAddRepoInput String
  | SubmitAddRepo
  | RemoveRepo String
  | HideItem String
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
  , interval :: Int
  , secondsLeft :: Int
  , filterText :: String
  , hasToken :: Boolean
  , expandedItems :: Set String
  , autoRefresh :: Boolean
  , repoList :: Array String
  , hiddenItems :: Set String
  , dragging :: Maybe String
  , showAddRepo :: Boolean
  , addRepoInput :: String
  }
