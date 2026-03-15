-- | Convenience re-export of the GitHub API client.
-- |
-- | The implementation is split into two sub-modules:
-- |
-- | - `GitHub.Rest`    — REST v3 API (repos, issues,
-- |   PRs, workflows, check-runs).
-- | - `GitHub.GraphQL` — GraphQL API (Projects v2
-- |   boards, items, mutations).
-- |
-- | Import this module for the full API surface, or
-- | import the sub-modules directly for a narrower
-- | dependency.
module GitHub
  ( module GitHub.Rest
  , module GitHub.GraphQL
  ) where

import GitHub.Rest
  ( RateLimit
  , ghFetch
  , fetchAllPages
  , fetchUserRepos
  , fetchRepoIssues
  , fetchIssue
  , fetchRepoPRs
  , fetchPR
  , fetchRepo
  , fetchCheckRuns
  , fetchCommitStatuses
  , fetchWorkflowRuns
  , fetchWorkflowJobs
  , fetchCommitPRs
  )
import GitHub.GraphQL
  ( ghGraphQL
  , fetchUserProjects
  , fetchProjectItems
  , updateItemStatus
  , addDraftItem
  , updateDraftItem
  , deleteProjectItem
  , renameProject
  )
