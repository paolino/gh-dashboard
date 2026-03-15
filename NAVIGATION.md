# gh-dashboard -- Codebase Navigation

A PureScript [Halogen](https://github.com/purescript-halogen/purescript-halogen) single-page app that renders a personal GitHub dashboard: repos, issues, PRs, CI checks, workflow runs, Projects v2 boards, and embedded Claude Code agent terminals.

---

## Table of Contents

1. [Architecture Overview](#architecture-overview)
2. [Domain Types](#domain-types)
3. [GitHub API Client](#github-api-client)
4. [Application State and Initialization](#application-state-and-initialization)
5. [Action Dispatch](#action-dispatch)
6. [Repo Action Handlers](#repo-action-handlers)
7. [Project Action Handlers](#project-action-handlers)
8. [Agent / Terminal Handlers](#agent--terminal-handlers)
9. [Shared Action Helpers](#shared-action-helpers)
10. [Refresh Logic](#refresh-logic)
11. [Repo Utilities](#repo-utilities)
12. [View Layer](#view-layer)
13. [Persistence](#persistence)
14. [FFI Modules](#ffi-modules)
15. [Directory Tree](#directory-tree)

---

## Architecture Overview

The app follows the standard Halogen component pattern with a single root component. Business logic is split across handler modules to keep `Main.purs` thin.

```
User clicks -> Action emitted -> Main.handleAction dispatches
                                      |
              +----------+------------+----------+
              |          |            |          |
         Action.Repos  Action.Projects  Action.Agent  (inline one-liners)
              |          |            |
         GitHub.Rest  GitHub.GraphQL  Fetch (agent HTTP)
```

Key design decisions:
- **Dispatch callback** -- handler modules receive a `Dispatch` function instead of importing each other, avoiding circular dependencies.
- **Optimistic updates** -- project status changes, renames, and deletes modify state before the API call, then roll back on error.
- **Lazy loading** -- detail sections (issues, PRs, workflows) fetch data only when first expanded.
- **Single expanded repo** -- only one repo can be expanded at a time; switching clears the previous detail.

---

## Domain Types

[`src/Types.purs`](https://github.com/lambdasistemi/gh-dashboard/blob/e129771/src/Types.purs#L1-L299) defines all domain newtypes and their `DecodeJson` instances.

| Type | Purpose | Key fields |
|------|---------|------------|
| [`Repo`](https://github.com/lambdasistemi/gh-dashboard/blob/e129771/src/Types.purs#L40-L52) | A GitHub repository | `fullName`, `ownerLogin`, `defaultBranch`, `openIssuesCount` |
| [`Issue`](https://github.com/lambdasistemi/gh-dashboard/blob/e129771/src/Types.purs#L86-L95) | An open issue (excludes PRs) | `number`, `title`, `labels`, `assignees`, `body` |
| [`PullRequest`](https://github.com/lambdasistemi/gh-dashboard/blob/e129771/src/Types.purs#L122-L133) | An open PR | `number`, `title`, `draft`, `headSha`, `labels` |
| [`CheckRun`](https://github.com/lambdasistemi/gh-dashboard/blob/e129771/src/Types.purs#L165-L170) | A CI check run | `name`, `status`, `conclusion` |
| [`WorkflowRun`](https://github.com/lambdasistemi/gh-dashboard/blob/e129771/src/Types.purs#L188-L197) | A workflow run on the default branch | `runId`, `name`, `headSha`, `displayTitle` |
| [`WorkflowJob`](https://github.com/lambdasistemi/gh-dashboard/blob/e129771/src/Types.purs#L230-L235) | A single job within a workflow run | `name`, `status`, `conclusion` |
| [`Project`](https://github.com/lambdasistemi/gh-dashboard/blob/e129771/src/Types.purs#L258-L263) | A Projects v2 board | `id`, `title`, `url`, `itemCount` |
| [`ProjectItem`](https://github.com/lambdasistemi/gh-dashboard/blob/e129771/src/Types.purs#L266-L277) | An item on a project board | `itemId`, `draftId`, `status`, `itemType`, `repoName`, `labels` |
| [`RepoDetail`](https://github.com/lambdasistemi/gh-dashboard/blob/e129771/src/Types.purs#L287-L298) | Cached detail for an expanded repo | `issues`, `pullRequests`, `prChecks`, `workflowRuns`, `workflowJobs` |
| [`Page`](https://github.com/lambdasistemi/gh-dashboard/blob/e129771/src/Types.purs#L253-L255) | Active top-level page | `ReposPage` or `ProjectsPage` |
| [`StatusField`](https://github.com/lambdasistemi/gh-dashboard/blob/e129771/src/Types.purs#L280-L284) | Status field metadata for a project | `fieldId`, `options` array of `{optionId, name}` |

Supporting type aliases: [`Label`](https://github.com/lambdasistemi/gh-dashboard/blob/e129771/src/Types.purs#L30-L32), [`Assignee`](https://github.com/lambdasistemi/gh-dashboard/blob/e129771/src/Types.purs#L35-L37), [`CommitPR`](https://github.com/lambdasistemi/gh-dashboard/blob/e129771/src/Types.purs#L224-L227).

---

## GitHub API Client

The API layer is split into REST and GraphQL, re-exported from a single facade module.

### Facade

[`src/GitHub.purs`](https://github.com/lambdasistemi/gh-dashboard/blob/e129771/src/GitHub.purs#L1-L44) re-exports everything from `GitHub.Rest` and `GitHub.GraphQL`.

### REST (v3)

[`src/GitHub/Rest.purs`](https://github.com/lambdasistemi/gh-dashboard/blob/e129771/src/GitHub/Rest.purs#L1-L485)

Core transport:

| Function | Lines | Purpose |
|----------|-------|---------|
| [`ghFetch`](https://github.com/lambdasistemi/gh-dashboard/blob/e129771/src/GitHub/Rest.purs#L84-L120) | 84-120 | Authenticated GET. Parses JSON, extracts rate-limit headers and `Link: rel="next"`. |
| [`parseLinkNext`](https://github.com/lambdasistemi/gh-dashboard/blob/e129771/src/GitHub/Rest.purs#L128-L161) | 128-161 | Extracts the next-page URL from RFC 5988 Link headers. |
| [`fetchAllPages`](https://github.com/lambdasistemi/gh-dashboard/blob/e129771/src/GitHub/Rest.purs#L168-L199) | 168-199 | Follows pagination to accumulate all items into a single array. |

Domain fetch functions:

| Function | Lines | Endpoint |
|----------|-------|----------|
| [`fetchUserRepos`](https://github.com/lambdasistemi/gh-dashboard/blob/e129771/src/GitHub/Rest.purs#L206-L224) | 206-224 | `GET /user/repos?affiliation=owner,collaborator` |
| [`fetchRepoIssues`](https://github.com/lambdasistemi/gh-dashboard/blob/e129771/src/GitHub/Rest.purs#L244-L263) | 244-263 | `GET /repos/:full/issues` (filters out PRs via `RawIssue` wrapper) |
| [`fetchIssue`](https://github.com/lambdasistemi/gh-dashboard/blob/e129771/src/GitHub/Rest.purs#L266-L281) | 266-281 | `GET /repos/:full/issues/:n` |
| [`fetchRepo`](https://github.com/lambdasistemi/gh-dashboard/blob/e129771/src/GitHub/Rest.purs#L284-L296) | 284-296 | `GET /repos/:full` |
| [`fetchRepoPRs`](https://github.com/lambdasistemi/gh-dashboard/blob/e129771/src/GitHub/Rest.purs#L299-L309) | 299-309 | `GET /repos/:full/pulls?state=open` |
| [`fetchPR`](https://github.com/lambdasistemi/gh-dashboard/blob/e129771/src/GitHub/Rest.purs#L312-L327) | 312-327 | `GET /repos/:full/pulls/:n` |
| [`fetchCheckRuns`](https://github.com/lambdasistemi/gh-dashboard/blob/e129771/src/GitHub/Rest.purs#L330-L351) | 330-351 | `GET /repos/:full/commits/:sha/check-runs` |
| [`fetchCommitStatuses`](https://github.com/lambdasistemi/gh-dashboard/blob/e129771/src/GitHub/Rest.purs#L359-L407) | 359-407 | `GET /repos/:full/commits/:sha/statuses` -- converts legacy statuses into `CheckRun` for uniform rendering |
| [`fetchWorkflowRuns`](https://github.com/lambdasistemi/gh-dashboard/blob/e129771/src/GitHub/Rest.purs#L411-L433) | 411-433 | `GET /repos/:full/actions/runs?branch=...` |
| [`fetchWorkflowJobs`](https://github.com/lambdasistemi/gh-dashboard/blob/e129771/src/GitHub/Rest.purs#L436-L455) | 436-455 | `GET /repos/:full/actions/runs/:id/jobs` |
| [`fetchCommitPRs`](https://github.com/lambdasistemi/gh-dashboard/blob/e129771/src/GitHub/Rest.purs#L458-L485) | 458-485 | `GET /repos/:full/commits/:sha/pulls` -- returns the first associated PR |

### GraphQL (Projects v2)

[`src/GitHub/GraphQL.purs`](https://github.com/lambdasistemi/gh-dashboard/blob/e129771/src/GitHub/GraphQL.purs#L1-L658)

Core transport:

| Function | Lines | Purpose |
|----------|-------|---------|
| [`ghGraphQL`](https://github.com/lambdasistemi/gh-dashboard/blob/e129771/src/GitHub/GraphQL.purs#L75-L106) | 75-106 | POST `{query, variables}` to `/graphql`. Extracts GraphQL-level errors. |
| [`ghMutation`](https://github.com/lambdasistemi/gh-dashboard/blob/e129771/src/GitHub/GraphQL.purs#L261-L267) | 261-267 | Wrapper for mutations that return no useful data -- eliminates boilerplate. |
| [`extractGraphQLErrors`](https://github.com/lambdasistemi/gh-dashboard/blob/e129771/src/GitHub/GraphQL.purs#L110-L128) | 110-128 | Parses the `errors` array from a GraphQL response. |

Queries and mutations:

| Function | Lines | GraphQL operation |
|----------|-------|-------------------|
| [`fetchUserProjects`](https://github.com/lambdasistemi/gh-dashboard/blob/e129771/src/GitHub/GraphQL.purs#L198-L207) | 198-207 | `viewer.projectsV2` -- light metadata only |
| [`fetchProjectItems`](https://github.com/lambdasistemi/gh-dashboard/blob/e129771/src/GitHub/GraphQL.purs#L213-L251) | 213-251 | Paginated items for a project, plus status field metadata |
| [`updateItemStatus`](https://github.com/lambdasistemi/gh-dashboard/blob/e129771/src/GitHub/GraphQL.purs#L270-L301) | 270-301 | `updateProjectV2ItemFieldValue` |
| [`addDraftItem`](https://github.com/lambdasistemi/gh-dashboard/blob/e129771/src/GitHub/GraphQL.purs#L304-L324) | 304-324 | `addProjectV2DraftIssue` |
| [`updateDraftItem`](https://github.com/lambdasistemi/gh-dashboard/blob/e129771/src/GitHub/GraphQL.purs#L327-L347) | 327-347 | `updateProjectV2DraftIssue` |
| [`deleteProjectItem`](https://github.com/lambdasistemi/gh-dashboard/blob/e129771/src/GitHub/GraphQL.purs#L372-L393) | 372-393 | `deleteProjectV2Item` |
| [`renameProject`](https://github.com/lambdasistemi/gh-dashboard/blob/e129771/src/GitHub/GraphQL.purs#L350-L370) | 350-370 | `updateProjectV2` |

Response navigation helpers ([L399-L658](https://github.com/lambdasistemi/gh-dashboard/blob/e129771/src/GitHub/GraphQL.purs#L399-L658)) manually traverse the nested GraphQL JSON to give clear error messages when the response shape changes. Key functions: `navigateProjects`, `navigateProjectItems`, `parseProjectItem`, `parseStatusField`, `extractFieldValue`, `extractLabels`.

---

## Application State and Initialization

### State

[`src/View/Types.purs`](https://github.com/lambdasistemi/gh-dashboard/blob/e129771/src/View/Types.purs#L74-L119) defines the full `State` record (44 fields).

| Field group | Fields | Purpose |
|-------------|--------|---------|
| Auth | `token`, `hasToken` | GitHub PAT |
| Repos page | `repos`, `repoList`, `expanded`, `details`, `detailLoading`, `loading` | Repo list, which one is expanded, cached detail |
| Feedback | `error`, `info`, `rateLimit` | Error messages, rate-limit display |
| Filters | `filterText`, `issueLabelFilters`, `prLabelFilters`, `workflowStatusFilters`, `projectRepoFilters`, `sessionFilters` | Various filter Sets |
| UI toggles | `expandedItems`, `hiddenItems`, `showAddRepo`, `addRepoInput`, `darkTheme`, `dragging` | Expand/collapse, hidden items, theme |
| Section loading | `issuesLoading`, `prsLoading`, `workflowsLoading`, `projectsLoading`, `projectItemsLoading` | Spinner flags |
| Projects page | `currentPage`, `projects`, `expandedProject`, `projectItems`, `projectStatusFields`, `newItemTitle`, `editingItem`, `editItemTitle`, `editingProject`, `editProjectTitle` | Projects v2 state |
| Agent | `agentServer`, `launchedItems`, `terminalKeys`, `terminalUrls`, `agentSessions` | Agent daemon integration |

### Initialization

[`Main.purs` Initialize handler](https://github.com/lambdasistemi/gh-dashboard/blob/e129771/src/Main.purs#L195-L231) runs on mount:

1. Load token, repo list, agent server URL, and view state from localStorage.
2. Apply the theme to `<body>`.
3. Restore all persisted view fields into state.
4. If a token exists, refresh agent sessions then either fetch repos (ReposPage) or projects (ProjectsPage).

[`initialState`](https://github.com/lambdasistemi/gh-dashboard/blob/e129771/src/Main.purs#L120-L165) sets every field to its empty/default value.

---

## Action Dispatch

[`src/View/Types.purs` Action](https://github.com/lambdasistemi/gh-dashboard/blob/e129771/src/View/Types.purs#L21-L72) is a flat sum type with 40+ constructors. The dispatcher in [`Main.handleAction`](https://github.com/lambdasistemi/gh-dashboard/blob/e129771/src/Main.purs#L185-L436) is a single `case _ of` that routes each constructor:

- **Initialization and auth** (L195-L248): `Initialize`, `SetToken`, `SubmitToken`
- **Repo actions** (L254-L283): delegated to `Action.Repos`
- **Inline one-liners** (L289-L381): `SetFilter`, `ToggleAddRepo`, `CopyText`, label/status filter toggles, `ToggleTheme`, `ExportStorage`, `ImportStorage`, `ResetAll`, `SwitchPage`
- **Project actions** (L386-L416): delegated to `Action.Projects`
- **Agent actions** (L422-L435): delegated to `Action.Agent`

The dispatcher passes itself as a `Dispatch` callback to handler modules that need cross-module action calls.

### Action Constructors

| Constructor | Parameters | Handler module |
|-------------|------------|----------------|
| `Initialize` | -- | Main (inline) |
| `SetToken` | `String` | Main (inline) |
| `SubmitToken` | -- | Main (inline) |
| `RefreshRepo` | `String` (fullName) | Action.Repos |
| `RefreshIssues` | -- | Action.Repos |
| `RefreshIssue` | `Int` (number) | Action.Repos |
| `RefreshPRs` | -- | Action.Repos |
| `RefreshPR` | `Int` (number) | Action.Repos |
| `RefreshWorkflows` | -- | Action.Repos |
| `WorkflowPrevSha` / `WorkflowNextSha` | -- | Action.Repos |
| `ToggleExpand` | `String` (fullName) | Action.Repos |
| `ToggleItem` | `String` (key) | Action.Repos |
| `DragStart` / `DragDrop` | `String` (fullName) | Action.Repos |
| `SubmitAddRepo` | -- | Action.Repos |
| `RemoveRepo` | `String` (fullName) | Action.Repos |
| `HideItem` | `String` (url) | Action.Repos |
| `SetFilter` | `String` | Main (inline) |
| `ToggleAddRepo` | -- | Main (inline) |
| `SetAddRepoInput` | `String` | Main (inline) |
| `CopyText` | `String` | Main (inline) |
| `ToggleIssueLabelFilter` | `String` | Main (inline) |
| `TogglePRLabelFilter` | `String` | Main (inline) |
| `ToggleWorkflowStatusFilter` | `String` | Main (inline) |
| `ToggleTheme` | -- | Main (inline) |
| `ExportStorage` / `ImportStorage` | -- | Main (inline) |
| `ResetAll` | -- | Main (inline) |
| `SwitchPage` | `Page` | Main (inline) |
| `RefreshProjects` | -- | Action.Projects |
| `ExpandProject` | `String` (projectId) | Action.Projects |
| `RefreshProjectItems` | `String` (projectId) | Action.Projects |
| `RefreshProjectItem` | `String String Int` | Action.Projects |
| `ToggleProjectRepoFilter` | `String` | Action.Projects |
| `SetItemStatus` | `String String String` | Action.Projects |
| `SetNewItemTitle` | `String` | Action.Projects |
| `SubmitNewItem` | `String` (projectId) | Action.Projects |
| `StartEditItem` | `String String` | Action.Projects |
| `SetEditItemTitle` | `String` | Action.Projects |
| `SubmitEditItem` | `String String String` | Action.Projects |
| `DeleteItem` | `String String` | Action.Projects |
| `StartRenameProject` | `String String` | Action.Projects |
| `SetRenameProjectTitle` | `String` | Action.Projects |
| `SubmitRenameProject` | `String String` | Action.Projects |
| `LaunchAgent` | `String String Int` | Action.Agent |
| `DetachAgent` | `String Int` | Action.Agent |
| `StopAgent` | `String Int` | Action.Agent |
| `SetAgentServer` | `String` | Action.Agent |
| `RefreshAgentSessions` | -- | Action.Agent |
| `ToggleSessionFilter` | `String` | Action.Agent |

---

## Repo Action Handlers

[`src/Action/Repos.purs`](https://github.com/lambdasistemi/gh-dashboard/blob/e129771/src/Action/Repos.purs#L1-L575)

### Data fetching

| Handler | Lines | Behavior |
|---------|-------|----------|
| [`handleRefreshRepo`](https://github.com/lambdasistemi/gh-dashboard/blob/e129771/src/Action/Repos.purs#L103-L113) | 103-113 | Re-fetch and upsert a single repo |
| [`handleRefreshIssues`](https://github.com/lambdasistemi/gh-dashboard/blob/e129771/src/Action/Repos.purs#L115-L134) | 115-134 | Fetch open issues for the expanded repo |
| [`handleRefreshIssue`](https://github.com/lambdasistemi/gh-dashboard/blob/e129771/src/Action/Repos.purs#L136-L157) | 136-157 | Refresh a single issue in-place |
| [`handleRefreshPRs`](https://github.com/lambdasistemi/gh-dashboard/blob/e129771/src/Action/Repos.purs#L159-L222) | 159-222 | Fetch open PRs, then check-runs + commit statuses per PR (skips hidden PRs) |
| [`handleRefreshPR`](https://github.com/lambdasistemi/gh-dashboard/blob/e129771/src/Action/Repos.purs#L224-L231) | 224-231 | Re-fetch a single PR and its checks via `Refresh.refreshSinglePR` |
| [`handleRefreshWorkflows`](https://github.com/lambdasistemi/gh-dashboard/blob/e129771/src/Action/Repos.purs#L233-L262) | 233-262 | Fetch workflow runs, extract unique SHAs, load jobs for the first SHA |
| [`handleWorkflowPrevSha` / `handleWorkflowNextSha`](https://github.com/lambdasistemi/gh-dashboard/blob/e129771/src/Action/Repos.purs#L264-L308) | 264-308 | Navigate between SHAs in the workflow viewer |
| [`loadWorkflowShaDetails`](https://github.com/lambdasistemi/gh-dashboard/blob/e129771/src/Action/Repos.purs#L504-L575) | 504-575 | Load jobs and PR info for the currently selected SHA |
| [`extractShas`](https://github.com/lambdasistemi/gh-dashboard/blob/e129771/src/Action/Repos.purs#L492-L500) | 492-500 | Deduplicate SHAs preserving order |

### UI interactions

| Handler | Lines | Behavior |
|---------|-------|----------|
| [`handleToggleExpand`](https://github.com/lambdasistemi/gh-dashboard/blob/e129771/src/Action/Repos.purs#L310-L333) | 310-333 | Expand/collapse a repo row. Clears details when switching repos. |
| [`handleToggleItem`](https://github.com/lambdasistemi/gh-dashboard/blob/e129771/src/Action/Repos.purs#L335-L390) | 335-390 | Expand/collapse a detail section. Detaches terminals on collapse. Auto-fetches on first open. |
| [`handleDragStart` / `handleDragDrop`](https://github.com/lambdasistemi/gh-dashboard/blob/e129771/src/Action/Repos.purs#L392-L414) | 392-414 | Drag-and-drop repo reordering |
| [`handleSubmitAddRepo`](https://github.com/lambdasistemi/gh-dashboard/blob/e129771/src/Action/Repos.purs#L416-L454) | 416-454 | Add a repo by URL or `owner/repo` name |
| [`handleRemoveRepo`](https://github.com/lambdasistemi/gh-dashboard/blob/e129771/src/Action/Repos.purs#L456-L481) | 456-481 | Confirm and remove a repo from the list |
| [`handleHideItem`](https://github.com/lambdasistemi/gh-dashboard/blob/e129771/src/Action/Repos.purs#L483-L489) | 483-489 | Toggle hide/unhide for an issue or PR |

---

## Project Action Handlers

[`src/Action/Projects.purs`](https://github.com/lambdasistemi/gh-dashboard/blob/e129771/src/Action/Projects.purs#L1-L453)

| Handler | Lines | Behavior |
|---------|-------|----------|
| [`handleRefreshProjects`](https://github.com/lambdasistemi/gh-dashboard/blob/e129771/src/Action/Projects.purs#L81-L100) | 81-100 | Fetch project list via GraphQL |
| [`handleExpandProject`](https://github.com/lambdasistemi/gh-dashboard/blob/e129771/src/Action/Projects.purs#L102-L125) | 102-125 | Toggle expand; lazy-loads items on first open |
| [`handleRefreshProjectItems`](https://github.com/lambdasistemi/gh-dashboard/blob/e129771/src/Action/Projects.purs#L127-L165) | 127-165 | Paginated fetch of items + status field metadata |
| [`handleRefreshProjectItem`](https://github.com/lambdasistemi/gh-dashboard/blob/e129771/src/Action/Projects.purs#L167-L204) | 167-204 | Re-fetch a single issue's title/body from REST |
| [`handleSetItemStatus`](https://github.com/lambdasistemi/gh-dashboard/blob/e129771/src/Action/Projects.purs#L216-L270) | 216-270 | **Optimistic** status change via `updateItemStatus` mutation |
| [`handleSubmitNewItem`](https://github.com/lambdasistemi/gh-dashboard/blob/e129771/src/Action/Projects.purs#L277-L293) | 277-293 | Create a draft issue then refresh items |
| [`handleSubmitEditItem`](https://github.com/lambdasistemi/gh-dashboard/blob/e129771/src/Action/Projects.purs#L311-L349) | 311-349 | **Optimistic** rename of a draft issue |
| [`handleDeleteItem`](https://github.com/lambdasistemi/gh-dashboard/blob/e129771/src/Action/Projects.purs#L351-L383) | 351-383 | Confirm, **optimistic** remove, then API delete |
| [`handleSubmitRenameProject`](https://github.com/lambdasistemi/gh-dashboard/blob/e129771/src/Action/Projects.purs#L401-L427) | 401-427 | **Optimistic** project rename |
| [`friendlyProjectError`](https://github.com/lambdasistemi/gh-dashboard/blob/e129771/src/Action/Projects.purs#L432-L453) | 432-453 | Rewrites `insufficient_scopes` errors into a user-friendly hint about `read:project` |

---

## Agent / Terminal Handlers

[`src/Action/Agent.purs`](https://github.com/lambdasistemi/gh-dashboard/blob/e129771/src/Action/Agent.purs#L1-L318)

The agent integration connects to an external daemon that runs Claude Code sessions against GitHub issues. Sessions are identified by `"owner/repo#issue"` keys.

| Handler | Lines | Behavior |
|---------|-------|----------|
| [`handleLaunchAgent`](https://github.com/lambdasistemi/gh-dashboard/blob/e129771/src/Action/Agent.purs#L81-L173) | 81-173 | POST `/sessions` to create a session, derive WebSocket URL, expand the item, attach an xterm.js terminal |
| [`handleDetachAgent`](https://github.com/lambdasistemi/gh-dashboard/blob/e129771/src/Action/Agent.purs#L175-L191) | 175-191 | Destroy the terminal widget without stopping the remote session |
| [`handleStopAgent`](https://github.com/lambdasistemi/gh-dashboard/blob/e129771/src/Action/Agent.purs#L193-L240) | 193-240 | Confirm, destroy terminal, DELETE `/sessions/:sid` |
| [`handleSetAgentServer`](https://github.com/lambdasistemi/gh-dashboard/blob/e129771/src/Action/Agent.purs#L242-L246) | 242-246 | Save agent server URL to state and localStorage |
| [`handleRefreshAgentSessions`](https://github.com/lambdasistemi/gh-dashboard/blob/e129771/src/Action/Agent.purs#L248-L275) | 248-275 | GET `/sessions`, parse into a `Map String String` of key-to-state |
| [`handleToggleSessionFilter`](https://github.com/lambdasistemi/gh-dashboard/blob/e129771/src/Action/Agent.purs#L277-L283) | 277-283 | Toggle "Worktree" / "Running" filter |
| [`reattachTerminals`](https://github.com/lambdasistemi/gh-dashboard/blob/e129771/src/Action/Agent.purs#L288-L300) | 288-300 | Re-opens xterm instances after Halogen re-renders may have destroyed container divs |
| [`parseSession`](https://github.com/lambdasistemi/gh-dashboard/blob/e129771/src/Action/Agent.purs#L304-L318) | 304-318 | Parse a session JSON object into `(key, state)` tuple |

---

## Shared Action Helpers

[`src/Action/Common.purs`](https://github.com/lambdasistemi/gh-dashboard/blob/e129771/src/Action/Common.purs#L1-L141)

| Export | Lines | Purpose |
|--------|-------|---------|
| [`Dispatch`](https://github.com/lambdasistemi/gh-dashboard/blob/e129771/src/Action/Common.purs#L50-L51) | 50-51 | Type alias for the cross-module action callback |
| [`HalogenAction`](https://github.com/lambdasistemi/gh-dashboard/blob/e129771/src/Action/Common.purs#L54-L55) | 54-55 | Shorthand for the Halogen action monad |
| [`toggleSet`](https://github.com/lambdasistemi/gh-dashboard/blob/e129771/src/Action/Common.purs#L61-L64) | 61-64 | Insert-or-remove from a Set. Used by every filter toggle. |
| [`persistView`](https://github.com/lambdasistemi/gh-dashboard/blob/e129771/src/Action/Common.purs#L71-L87) | 71-87 | Save current view state to localStorage |
| [`updateDetail`](https://github.com/lambdasistemi/gh-dashboard/blob/e129771/src/Action/Common.purs#L102-L112) | 102-112 | Modify the current `RepoDetail`, creating an empty one if none exists |
| [`guardExpanded`](https://github.com/lambdasistemi/gh-dashboard/blob/e129771/src/Action/Common.purs#L118-L125) | 118-125 | Run an action only if the given repo is still expanded (guards against stale async results) |
| [`emptyDetail`](https://github.com/lambdasistemi/gh-dashboard/blob/e129771/src/Action/Common.purs#L129-L141) | 129-141 | Blank `RepoDetail` record |
| [`termElementId`](https://github.com/lambdasistemi/gh-dashboard/blob/e129771/src/Action/Common.purs#L92-L96) | 92-96 | Converts `"owner/repo#42"` to a safe DOM ID `"term-owner-repo-42"` |

---

## Refresh Logic

[`src/Refresh.purs`](https://github.com/lambdasistemi/gh-dashboard/blob/e129771/src/Refresh.purs#L1-L135)

| Function | Lines | Purpose |
|----------|-------|---------|
| [`doRefresh`](https://github.com/lambdasistemi/gh-dashboard/blob/e129771/src/Refresh.purs#L30-L72) | 30-72 | If `repoList` is empty, seeds from API (top 25 repos). Otherwise re-fetches each repo individually and reorders. |
| [`refreshSinglePR`](https://github.com/lambdasistemi/gh-dashboard/blob/e129771/src/Refresh.purs#L75-L135) | 75-135 | Re-fetch a PR + its check-runs and commit statuses, merge into existing detail state |

---

## Repo Utilities

[`src/RepoUtils.purs`](https://github.com/lambdasistemi/gh-dashboard/blob/e129771/src/RepoUtils.purs#L1-L90) -- pure helpers for repo list manipulation.

| Function | Lines | Purpose |
|----------|-------|---------|
| [`applyFilter`](https://github.com/lambdasistemi/gh-dashboard/blob/e129771/src/RepoUtils.purs#L21-L34) | 21-34 | Filter repos by name or description (case-insensitive) |
| [`parseRepoName`](https://github.com/lambdasistemi/gh-dashboard/blob/e129771/src/RepoUtils.purs#L37-L53) | 37-53 | Extract `owner/repo` from a GitHub URL or plain name |
| [`upsertRepo`](https://github.com/lambdasistemi/gh-dashboard/blob/e129771/src/RepoUtils.purs#L56-L68) | 56-68 | Insert or update a repo in the array |
| [`orderRepos`](https://github.com/lambdasistemi/gh-dashboard/blob/e129771/src/RepoUtils.purs#L71-L77) | 71-77 | Reorder repos to match the stored list |
| [`moveItem`](https://github.com/lambdasistemi/gh-dashboard/blob/e129771/src/RepoUtils.purs#L80-L89) | 80-89 | Move an item before another (used by drag-and-drop) |

---

## View Layer

### Top-level View

[`src/View.purs`](https://github.com/lambdasistemi/gh-dashboard/blob/e129771/src/View.purs#L1-L276)

| Function | Lines | Purpose |
|----------|-------|---------|
| [`renderTokenForm`](https://github.com/lambdasistemi/gh-dashboard/blob/e129771/src/View.purs#L22-L88) | 22-88 | Token input form with "Getting started" instructions |
| [`renderDashboard`](https://github.com/lambdasistemi/gh-dashboard/blob/e129771/src/View.purs#L91-L137) | 91-137 | Main dashboard: toolbar, add-repo bar, error, page content |
| [`renderToolbar`](https://github.com/lambdasistemi/gh-dashboard/blob/e129771/src/View.purs#L140-L250) | 140-250 | Tab bar (Repos/Projects), filter input, agent server input, theme toggle, export/import/reset buttons |

### View Types

[`src/View/Types.purs`](https://github.com/lambdasistemi/gh-dashboard/blob/e129771/src/View/Types.purs#L1-L119) defines `Action` (40+ constructors) and `State` (44 fields). These are shared across all view sub-modules.

### Repo Table

[`src/View/RepoTable.purs`](https://github.com/lambdasistemi/gh-dashboard/blob/e129771/src/View/RepoTable.purs#L1-L181)

Renders the repo list as a table with columns: drag handle, actions, name, description, language, visibility, issues, updated date. Each row is clickable (expand/collapse) and draggable (reorder). Expanded repos show the detail panel below.

Helper renderers: [`renderLangBadge`](https://github.com/lambdasistemi/gh-dashboard/blob/e129771/src/View/RepoTable.purs#L140-L148), [`renderVisBadge`](https://github.com/lambdasistemi/gh-dashboard/blob/e129771/src/View/RepoTable.purs#L150-L164), [`renderCountBadge`](https://github.com/lambdasistemi/gh-dashboard/blob/e129771/src/View/RepoTable.purs#L167-L181).

### Detail Panel

[`src/View/Detail.purs`](https://github.com/lambdasistemi/gh-dashboard/blob/e129771/src/View/Detail.purs#L1-L53) -- composes the three detail sections (workflows, issues, PRs) into a single panel below the expanded repo row.

### Issues Section

[`src/View/Issues.purs`](https://github.com/lambdasistemi/gh-dashboard/blob/e129771/src/View/Issues.purs#L1-L291)

- [`renderIssuesSection`](https://github.com/lambdasistemi/gh-dashboard/blob/e129771/src/View/Issues.purs#L38-L181): Collapsible section with label filter, visible/hidden partitions
- [`renderIssueRow`](https://github.com/lambdasistemi/gh-dashboard/blob/e129771/src/View/Issues.purs#L184-L291): Single issue row with agent badges (worktree, running), expandable body/terminal

### PRs Section

[`src/View/PRs.purs`](https://github.com/lambdasistemi/gh-dashboard/blob/e129771/src/View/PRs.purs#L1-L438)

- [`renderPRsSection`](https://github.com/lambdasistemi/gh-dashboard/blob/e129771/src/View/PRs.purs#L44-L212): Collapsible section with label + CI status filter, visible/hidden partitions
- [`renderPRRow`](https://github.com/lambdasistemi/gh-dashboard/blob/e129771/src/View/PRs.purs#L215-L338): Single PR row with draft tag, CI status badge, expandable body + failed checks side-by-side
- [`combineCheckRuns`](https://github.com/lambdasistemi/gh-dashboard/blob/e129771/src/View/PRs.purs#L341-L357): Derives combined CI status from check runs (pending/failure/cancelled/success/mixed)
- [`renderCheckRun`](https://github.com/lambdasistemi/gh-dashboard/blob/e129771/src/View/PRs.purs#L414-L438): Single check run with status badge and link

### Workflows Section

[`src/View/Workflows.purs`](https://github.com/lambdasistemi/gh-dashboard/blob/e129771/src/View/Workflows.purs#L1-L351)

- [`renderWorkflowsSection`](https://github.com/lambdasistemi/gh-dashboard/blob/e129771/src/View/Workflows.purs#L64-L187): Collapsible section with SHA navigation, status filter, workflow table
- [`renderShaNav`](https://github.com/lambdasistemi/gh-dashboard/blob/e129771/src/View/Workflows.purs#L190-L250): Prev/next SHA navigation bar with commit link and associated PR
- [`renderWorkflowRow`](https://github.com/lambdasistemi/gh-dashboard/blob/e129771/src/View/Workflows.purs#L253-L310): Single workflow run with failed job sub-rows

### Projects View

[`src/View/Projects.purs`](https://github.com/lambdasistemi/gh-dashboard/blob/e129771/src/View/Projects.purs#L1-L856)

- [`renderProjects`](https://github.com/lambdasistemi/gh-dashboard/blob/e129771/src/View/Projects.purs#L34-L81): Expandable project table
- [`renderProjectRow`](https://github.com/lambdasistemi/gh-dashboard/blob/e129771/src/View/Projects.purs#L84-L182): Project row with rename inline edit
- [`renderProjectDetail`](https://github.com/lambdasistemi/gh-dashboard/blob/e129771/src/View/Projects.purs#L185-L248): Detail panel with new-item form, session filter, repo filter, status-grouped items
- [`renderRepoFilter`](https://github.com/lambdasistemi/gh-dashboard/blob/e129771/src/View/Projects.purs#L402-L441): Collapsible org-grouped repo filter tree
- [`groupByStatus`](https://github.com/lambdasistemi/gh-dashboard/blob/e129771/src/View/Projects.purs#L514-L535): Groups items by status column (Backlog, Todo, In Progress, Done, Stale)
- [`renderStatusSection`](https://github.com/lambdasistemi/gh-dashboard/blob/e129771/src/View/Projects.purs#L538-L600): Collapsible status column with item rows
- [`renderItemRow`](https://github.com/lambdasistemi/gh-dashboard/blob/e129771/src/View/Projects.purs#L603-L856): Project item row with status selector, agent badges, inline edit, expandable body/terminal
- [`applySessionFilter`](https://github.com/lambdasistemi/gh-dashboard/blob/e129771/src/View/Projects.purs#L306-L343): Filters items by Worktree/Running session state

### Detail Widgets

[`src/View/DetailWidgets.purs`](https://github.com/lambdasistemi/gh-dashboard/blob/e129771/src/View/DetailWidgets.purs#L1-L160) -- reusable buttons and selectors shared across Issues, PRs, and Projects views.

| Widget | Lines | Purpose |
|--------|-------|---------|
| [`refreshButton`](https://github.com/lambdasistemi/gh-dashboard/blob/e129771/src/View/DetailWidgets.purs#L23-L33) | 23-33 | Refresh icon button |
| [`copyButton`](https://github.com/lambdasistemi/gh-dashboard/blob/e129771/src/View/DetailWidgets.purs#L36-L46) | 36-46 | Copy-to-clipboard button |
| [`hideButton`](https://github.com/lambdasistemi/gh-dashboard/blob/e129771/src/View/DetailWidgets.purs#L49-L60) | 49-60 | Hide/unhide toggle |
| [`launchButton`](https://github.com/lambdasistemi/gh-dashboard/blob/e129771/src/View/DetailWidgets.purs#L63-L106) | 63-106 | Launch/detach/stop agent buttons |
| [`collectLabels`](https://github.com/lambdasistemi/gh-dashboard/blob/e129771/src/View/DetailWidgets.purs#L109-L124) | 109-124 | Collect unique label names with counts |
| [`renderLabelSelector`](https://github.com/lambdasistemi/gh-dashboard/blob/e129771/src/View/DetailWidgets.purs#L127-L160) | 127-160 | Multi-select label/status filter bar |

### View Helpers

[`src/View/Helpers.purs`](https://github.com/lambdasistemi/gh-dashboard/blob/e129771/src/View/Helpers.purs#L1-L195)

| Helper | Lines | Purpose |
|--------|-------|---------|
| [`renderMarkdownRow`](https://github.com/lambdasistemi/gh-dashboard/blob/e129771/src/View/Helpers.purs#L74-L98) | 74-98 | Table row with markdown-rendered body (uses `marked.js` via FFI) |
| [`renderTerminalRow`](https://github.com/lambdasistemi/gh-dashboard/blob/e129771/src/View/Helpers.purs#L45-L71) | 45-71 | Table row with terminal container div + resize handle |
| [`linkButton`](https://github.com/lambdasistemi/gh-dashboard/blob/e129771/src/View/Helpers.purs#L101-L109) | 101-109 | "Open on GitHub" link |
| [`detailHead`](https://github.com/lambdasistemi/gh-dashboard/blob/e129771/src/View/Helpers.purs#L112-L122) | 112-122 | Standard column headers for detail tables |
| [`renderAssignees`](https://github.com/lambdasistemi/gh-dashboard/blob/e129771/src/View/Helpers.purs#L125-L152) | 125-152 | Comma-separated assignee links |
| [`renderAuthor`](https://github.com/lambdasistemi/gh-dashboard/blob/e129771/src/View/Helpers.purs#L155-L165) | 155-165 | Author link to GitHub profile |
| [`renderLabels`](https://github.com/lambdasistemi/gh-dashboard/blob/e129771/src/View/Helpers.purs#L168-L186) | 168-186 | Label tag spans |
| [`formatDate`](https://github.com/lambdasistemi/gh-dashboard/blob/e129771/src/View/Helpers.purs#L189-L190) | 189-190 | ISO date to `YYYY-MM-DD` |
| [`formatDateTime`](https://github.com/lambdasistemi/gh-dashboard/blob/e129771/src/View/Helpers.purs#L193-L194) | 193-194 | ISO date to `YYYY-MM-DD HH:MM` |
| [`parseMarkdownImpl`](https://github.com/lambdasistemi/gh-dashboard/blob/e129771/src/View/Helpers.purs#L32) | 32 | Foreign import to `marked.parse()` |

---

## Persistence

[`src/Storage.purs`](https://github.com/lambdasistemi/gh-dashboard/blob/e129771/src/Storage.purs#L1-L256)

### localStorage Keys

| Key | Type | Purpose |
|-----|------|---------|
| `gh-dashboard-token` | `String` | GitHub PAT |
| `gh-dashboard-repos` | `JSON Array<String>` | Ordered repo full names |
| `gh-dashboard-view` | `JSON Object` | Full view state (see below) |
| `gh-dashboard-agent-server` | `String` | Agent daemon URL |

### ViewState

[`ViewState` type](https://github.com/lambdasistemi/gh-dashboard/blob/e129771/src/Storage.purs#L38-L50) captures everything about what is visible:

| Field | Type | Default |
|-------|------|---------|
| `currentPage` | `Page` | `ReposPage` |
| `expanded` | `Maybe String` | `Nothing` |
| `expandedProject` | `Maybe String` | `Nothing` |
| `expandedItems` | `Set String` | `empty` |
| `filterText` | `String` | `""` |
| `hiddenItems` | `Set String` | `empty` |
| `darkTheme` | `Boolean` | `true` |
| `issueLabelFilters` | `Set String` | `empty` |
| `prLabelFilters` | `Set String` | `empty` |
| `workflowStatusFilters` | `Set String` | `empty` |
| `projectRepoFilters` | `Set String` | `empty` |

### Storage Functions

| Function | Lines | Purpose |
|----------|-------|---------|
| [`loadToken` / `saveToken`](https://github.com/lambdasistemi/gh-dashboard/blob/e129771/src/Storage.purs#L76-L86) | 76-86 | Read/write token |
| [`loadRepoList` / `saveRepoList`](https://github.com/lambdasistemi/gh-dashboard/blob/e129771/src/Storage.purs#L88-L109) | 88-109 | Read/write repo name array |
| [`loadViewState` / `saveViewState`](https://github.com/lambdasistemi/gh-dashboard/blob/e129771/src/Storage.purs#L129-L232) | 129-232 | Read/write full view state as JSON |
| [`loadAgentServer` / `saveAgentServer`](https://github.com/lambdasistemi/gh-dashboard/blob/e129771/src/Storage.purs#L237-L247) | 237-247 | Read/write agent server URL |
| [`clearAll`](https://github.com/lambdasistemi/gh-dashboard/blob/e129771/src/Storage.purs#L249-L256) | 249-256 | Remove token, repos, and view keys |

---

## FFI Modules

Each FFI module pairs a PureScript declaration file with a JavaScript implementation.

### Clipboard

- [`src/FFI/Clipboard.purs`](https://github.com/lambdasistemi/gh-dashboard/blob/e129771/src/FFI/Clipboard.purs#L1-L9) / [`src/FFI/Clipboard.js`](https://github.com/lambdasistemi/gh-dashboard/blob/e129771/src/FFI/Clipboard.js#L1-L3)
- `copyToClipboard :: String -> Effect Unit` -- calls `navigator.clipboard.writeText`

### Dialog

- [`src/FFI/Dialog.purs`](https://github.com/lambdasistemi/gh-dashboard/blob/e129771/src/FFI/Dialog.purs#L1-L7) / [`src/FFI/Dialog.js`](https://github.com/lambdasistemi/gh-dashboard/blob/e129771/src/FFI/Dialog.js#L1-L3)
- `confirmDialog :: String -> Effect Boolean` -- calls `window.confirm`

### Terminal (xterm.js)

- [`src/FFI/Terminal.purs`](https://github.com/lambdasistemi/gh-dashboard/blob/e129771/src/FFI/Terminal.purs#L1-L26) / [`src/FFI/Terminal.js`](https://github.com/lambdasistemi/gh-dashboard/blob/e129771/src/FFI/Terminal.js#L1-L139)
- `attachTerminal :: String -> String -> String -> Effect Unit` -- creates an xterm.js instance in a DOM element, connects via WebSocket, handles resize (window + drag handle)
- `destroyTerminal :: String -> Effect Unit` -- tears down a terminal by element ID
- `destroyOrphanedTerminals :: Effect (Array String)` -- cleans up terminals whose DOM container was removed
- Internal `_terminals` object tracks all active terminals

### Theme

- [`src/FFI/Theme.purs`](https://github.com/lambdasistemi/gh-dashboard/blob/e129771/src/FFI/Theme.purs#L1-L9) / [`src/FFI/Theme.js`](https://github.com/lambdasistemi/gh-dashboard/blob/e129771/src/FFI/Theme.js#L1-L3)
- `setBodyTheme :: Boolean -> Effect Unit` -- toggles `light-theme` class on `<body>`

### Storage Export/Import

- [`src/FFI/Storage.purs`](https://github.com/lambdasistemi/gh-dashboard/blob/e129771/src/FFI/Storage.purs#L1-L15) / [`src/FFI/Storage.js`](https://github.com/lambdasistemi/gh-dashboard/blob/e129771/src/FFI/Storage.js#L1-L51)
- `exportStorage :: Effect Unit` -- downloads non-token settings as a JSON file
- `importStorage :: Effect Unit` -- opens file picker, restores settings, reloads page
- Operates on a hardcoded `KEYS` list (repos, hidden, dark-theme, issue-labels, pr-labels)

### Markdown

- [`src/View/Helpers.js`](https://github.com/lambdasistemi/gh-dashboard/blob/e129771/src/View/Helpers.js#L1-L6) -- `parseMarkdownImpl` calls `marked.parse()` if the `marked` library is loaded

---

## Directory Tree

```
src/
  Main.purs                   -- Entry point, component, action dispatcher
  Types.purs                  -- Domain newtypes + DecodeJson instances
  Refresh.purs                -- Repo/PR refresh logic
  RepoUtils.purs              -- Pure repo list helpers (filter, reorder, parse)
  Storage.purs                -- localStorage read/write (token, repos, view, agent)
  GitHub.purs                 -- Re-export facade for REST + GraphQL
  GitHub/
    Rest.purs                 -- REST v3 client (ghFetch, pagination, domain fetchers)
    GraphQL.purs              -- GraphQL client (projects, mutations, response nav)
  Action/
    Common.purs               -- Shared helpers (Dispatch, toggleSet, persistView, etc.)
    Repos.purs                -- Repo page handlers (issues, PRs, workflows, drag-drop)
    Projects.purs             -- Project page handlers (CRUD, optimistic updates)
    Agent.purs                -- Agent/terminal handlers (launch, detach, stop, sessions)
  View.purs                   -- Top-level render (token form, dashboard, toolbar)
  View/
    Types.purs                -- Action sum type + State record
    RepoTable.purs            -- Repo table rows + badges
    Detail.purs               -- Detail panel compositor
    Issues.purs               -- Issues section (filter, visible/hidden, rows)
    PRs.purs                  -- PRs section (CI badges, checks, body)
    Workflows.purs            -- Workflows section (SHA nav, run/job rows)
    Projects.purs             -- Projects view (board table, status columns, item rows)
    DetailWidgets.purs        -- Reusable buttons (refresh, copy, hide, launch, labels)
    Helpers.purs              -- Shared renderers (markdown, terminal, assignees, dates)
    Helpers.js                -- marked.js FFI
  FFI/
    Clipboard.purs + .js      -- navigator.clipboard
    Dialog.purs + .js         -- window.confirm
    Terminal.purs + .js       -- xterm.js + WebSocket terminals
    Theme.purs + .js          -- body class toggle
    Storage.purs + .js        -- JSON export/import of settings
```
