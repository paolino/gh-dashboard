-- | Common action helpers and shared patterns.
-- |
-- | This module provides reusable building blocks for
-- | action handlers, reducing boilerplate across the
-- | Repos, Projects, and Agent handler modules.
-- |
-- | **Key abstractions:**
-- |
-- | - `Dispatch`: a callback for cross-module action
-- |   invocation (avoids circular imports between
-- |   handler modules).
-- |
-- | - `toggleSet`: the "toggle membership in a Set"
-- |   pattern used by all filter actions.
-- |
-- | - `persistView`: save the current view state to
-- |   localStorage (called after most user interactions).
-- |
-- | - `emptyDetail`: a blank `RepoDetail` record,
-- |   used as the initial value when expanding a repo
-- |   for the first time.
module Action.Common
  ( Dispatch
  , HalogenAction
  , toggleSet
  , persistView
  , emptyDetail
  , termElementId
  , updateDetail
  , guardExpanded
  ) where

import Prelude

import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Data.String (Pattern(..), Replacement(..), replaceAll)
import Effect.Class (liftEffect)
import Halogen as H
import Effect.Aff (Aff)
import Storage (saveViewState)
import Types (RepoDetail)
import View.Types (Action, State)

-- | A function that dispatches an `Action` within the
-- | Halogen monad. Passed to handler modules so they
-- | can trigger actions defined in other modules
-- | without circular imports.
type Dispatch o =
  Action -> H.HalogenM State Action () o Aff Unit

-- | Shorthand for the Halogen action monad.
type HalogenAction o =
  H.HalogenM State Action () o Aff Unit

-- | Toggle membership of a value in a Set.
-- |
-- | If the value is present it is removed; if absent
-- | it is inserted. Used by every filter toggle action.
toggleSet :: forall a. Ord a => a -> Set.Set a -> Set.Set a
toggleSet x s =
  if Set.member x s then Set.delete x s
  else Set.insert x s

-- | Save current view state to localStorage.
-- |
-- | Called after most user-facing state changes
-- | (filter, expand, theme, page switch) so the
-- | UI can be restored on next page load.
persistView :: forall o. HalogenAction o
persistView = do
  st <- H.get
  liftEffect $ saveViewState
    { currentPage: st.currentPage
    , expanded: st.expanded
    , expandedProject: st.expandedProject
    , expandedItems: st.expandedItems
    , filterText: st.filterText
    , hiddenItems: st.hiddenItems
    , darkTheme: st.darkTheme
    , issueLabelFilters: st.issueLabelFilters
    , prLabelFilters: st.prLabelFilters
    , workflowStatusFilters:
        st.workflowStatusFilters
    , projectRepoFilters: st.projectRepoFilters
    }

-- | Convert a launch key (e.g. "owner/repo#42") to a
-- | safe DOM element ID by replacing `/` and `#` with
-- | `-`.
termElementId :: String -> String
termElementId key =
  "term-"
    <> replaceAll (Pattern "/") (Replacement "-")
      (replaceAll (Pattern "#") (Replacement "-") key)

-- | Modify the current repo detail, creating an empty
-- | one if none exists yet. This eliminates the
-- | repeated Nothing/Just case split that appears in
-- | every fetch handler.
updateDetail
  :: forall o
   . (RepoDetail -> RepoDetail)
  -> HalogenAction o
updateDetail f = do
  st <- H.get
  case st.details of
    Nothing ->
      H.modify_ _ { details = Just (f emptyDetail) }
    Just detail ->
      H.modify_ _ { details = Just (f detail) }

-- | Run an action only if the given repo is still
-- | expanded. Used after async fetches to avoid
-- | updating stale state when the user collapsed
-- | the repo while the request was in flight.
guardExpanded
  :: forall o
   . String
  -> HalogenAction o
  -> HalogenAction o
guardExpanded fullName action = do
  st <- H.get
  when (st.expanded == Just fullName) action

-- | An empty repo detail record, used when opening
-- | a section before any data has been fetched.
emptyDetail :: RepoDetail
emptyDetail =
  { issues: []
  , pullRequests: []
  , issueCount: 0
  , prCount: 0
  , prChecks: Map.empty
  , workflowRuns: []
  , workflowCount: 0
  , workflowJobs: Map.empty
  , workflowShaIndex: 0
  , workflowShaPRs: Map.empty
  }
