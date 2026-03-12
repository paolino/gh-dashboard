# 🗺️ GH Dashboard — Semantic Navigation

A human-readable map of the codebase. Each section explains *what* the code does and *why*, with links to the source.

---

## Table of Contents

1. [🔑 Authentication](#-authentication)
2. [🌐 GitHub API Client](#-github-api-client)
   - [Core Fetching](#core-fetching)
   - [API Endpoints](#api-endpoints)
3. [📦 Domain Types](#-domain-types)
   - [Repo](#repo) · [Issue](#issue) · [PullRequest](#pullrequest) · [CheckRun](#checkrun) · [RepoDetail](#repodetail)
4. [🔄 Application State](#-application-state)
   - [State Fields](#state-fields)
   - [Initialization](#initialization)
5. [⚡ User Actions](#-user-actions)
   - [Data Fetching](#data-fetching)
   - [Refresh Logic](#refresh-logic)
   - [UI Interactions](#ui-interactions)
6. [🎨 View Layer](#-view-layer)
   - [Top Level](#top-level) · [Toolbar](#toolbar) · [Repo Table](#repo-table) · [Issue List](#issue-list) · [PR List](#pr-list)
7. [💾 Persistence Layer](#-persistence-layer)
   - [Storage Keys](#storage-keys)
   - [Operations](#operations)
8. [🔌 FFI (JavaScript Interop)](#-ffi-javascript-interop)
9. [🛠️ Utilities](#️-utilities)
10. [📁 Project Structure](#-project-structure)

---

## 🔑 Authentication

The app requires a GitHub personal access token to function. On first visit, users see a form to enter their token. Once submitted, the token is stored in localStorage and used for all API calls.

- **Token form UI**: Password input, connect button, and setup instructions with link to GitHub token creation page
  → [View.purs:21-84](https://github.com/lambdasistemi/gh-dashboard/blob/main/src/View.purs#L21-L84)

- **Token validation**: If empty, shows error message; otherwise saves and triggers initial data fetch
  → [Main.purs:136-148](https://github.com/lambdasistemi/gh-dashboard/blob/main/src/Main.purs#L136-L148)

- **Token persistence**: Stored under key `gh-dashboard-token` in browser localStorage
  → [Storage.purs:52-62](https://github.com/lambdasistemi/gh-dashboard/blob/main/src/Storage.purs#L52-L62)

---

## 🌐 GitHub API Client

REST client that handles authentication, pagination, rate limiting, and JSON decoding.

### Core Fetching

- **ghFetch**: Low-level fetch wrapper. Adds Bearer token header, parses JSON response, extracts rate-limit info from headers, parses Link header for pagination.
  → [GitHub.purs:51-87](https://github.com/lambdasistemi/gh-dashboard/blob/main/src/GitHub.purs#L51-L87)

- **Link header parsing**: Scans for `rel="next"` to find next page URL for paginated endpoints.
  → [GitHub.purs:90-123](https://github.com/lambdasistemi/gh-dashboard/blob/main/src/GitHub.purs#L90-L123)

- **fetchAllPages**: Recursively follows pagination links, accumulating results until no more pages.
  → [GitHub.purs:126-157](https://github.com/lambdasistemi/gh-dashboard/blob/main/src/GitHub.purs#L126-L157)

### API Endpoints

- **User repos**: Fetches repos where user is owner or collaborator, sorted by update time, max 100 per page.
  → [GitHub.purs:159-178](https://github.com/lambdasistemi/gh-dashboard/blob/main/src/GitHub.purs#L159-L178)

- **Repo issues**: Fetches open issues, filters out pull requests (GitHub API returns PRs in issues endpoint).
  → [GitHub.purs:195-214](https://github.com/lambdasistemi/gh-dashboard/blob/main/src/GitHub.purs#L195-L214)

- **Single issue**: Fetches one issue by number for refresh.
  → [GitHub.purs:217-232](https://github.com/lambdasistemi/gh-dashboard/blob/main/src/GitHub.purs#L217-L232)

- **Repo PRs**: Fetches open pull requests with pagination.
  → [GitHub.purs:250-260](https://github.com/lambdasistemi/gh-dashboard/blob/main/src/GitHub.purs#L250-L260)

- **Single PR**: Fetches one PR by number for refresh.
  → [GitHub.purs:263-278](https://github.com/lambdasistemi/gh-dashboard/blob/main/src/GitHub.purs#L263-L278)

- **Check runs**: Fetches GitHub Actions check runs for a commit SHA.
  → [GitHub.purs:281-302](https://github.com/lambdasistemi/gh-dashboard/blob/main/src/GitHub.purs#L281-L302)

- **Commit statuses**: Fetches legacy commit statuses, converted to CheckRun format for unified display.
  → [GitHub.purs:305-353](https://github.com/lambdasistemi/gh-dashboard/blob/main/src/GitHub.purs#L305-L353)

---

## 📦 Domain Types

Data models decoded from GitHub API responses.

### Repo

Repository metadata: id, name, full name (owner/repo), URL, description, language, visibility (public/private), open issues count, last update timestamp, owner login.

→ [Types.purs:33-72](https://github.com/lambdasistemi/gh-dashboard/blob/main/src/Types.purs#L33-L72)

### Issue

Open issue: number, title, URL, creation date, author login, labels array, assignees array, body text.

→ [Types.purs:75-108](https://github.com/lambdasistemi/gh-dashboard/blob/main/src/Types.purs#L75-L108)

### PullRequest

Like Issue but adds: draft boolean (for draft PRs), headSha (commit SHA for fetching CI status).

→ [Types.purs:111-151](https://github.com/lambdasistemi/gh-dashboard/blob/main/src/Types.purs#L111-L151)

### CheckRun

CI check result: name, status (queued/in_progress/completed), conclusion (success/failure/etc), URL to check details.

→ [Types.purs:154-174](https://github.com/lambdasistemi/gh-dashboard/blob/main/src/Types.purs#L154-L174)

### RepoDetail

Cached detail when repo is expanded: issues array, PRs array, counts, map of PR number to check runs.

→ [Types.purs:177-183](https://github.com/lambdasistemi/gh-dashboard/blob/main/src/Types.purs#L177-L183)

---

## 🔄 Application State

Halogen component with centralized state record.

### State Fields

| Field | Purpose |
|-------|---------|
| `token`, `hasToken` | GitHub token and auth status |
| `repos` | Fetched repo data array |
| `repoList` | Ordered repo names (user's custom order) |
| `expanded` | Currently expanded repo name |
| `details` | Cached issues/PRs for expanded repo |
| `expandedItems` | Expanded UI accordion sections |
| `hiddenItems` | Hidden issue/PR URLs |
| `filterText` | Search filter |
| `loading`, `issuesLoading`, `prsLoading` | Loading states |
| `error` | Current error message |
| `rateLimit` | GitHub rate limit info |
| `darkTheme` | Theme preference |
| `issueLabelFilters`, `prLabelFilters` | Active label filters |
| `dragging` | Repo being dragged |
| `showAddRepo`, `addRepoInput` | Add repo form state |

→ [Main.purs:75-98](https://github.com/lambdasistemi/gh-dashboard/blob/main/src/Main.purs#L75-L98)

### Initialization

On mount: loads token, repo list, hidden items, theme, and label filters from localStorage. If token exists, triggers full refresh.

→ [Main.purs:113-133](https://github.com/lambdasistemi/gh-dashboard/blob/main/src/Main.purs#L113-L133)

---

## ⚡ User Actions

All user interactions handled by `handleAction`.

### Data Fetching

- **Initialize**: Load persisted state, trigger refresh if token exists.
  → [Main.purs:113-133](https://github.com/lambdasistemi/gh-dashboard/blob/main/src/Main.purs#L113-L133)

- **RefreshRepo**: Fetch single repo metadata and upsert into list.
  → [Main.purs:149-157](https://github.com/lambdasistemi/gh-dashboard/blob/main/src/Main.purs#L149-L157)

- **RefreshIssues**: Fetch all open issues for expanded repo.
  → [Main.purs:158-190](https://github.com/lambdasistemi/gh-dashboard/blob/main/src/Main.purs#L158-L190)

- **RefreshPRs**: Fetch all open PRs with their check runs. Fetches checks only for visible (non-hidden) PRs.
  → [Main.purs:228-320](https://github.com/lambdasistemi/gh-dashboard/blob/main/src/Main.purs#L228-L320)

- **RefreshPR**: Refresh single PR and its checks.
  → [Main.purs:321-326](https://github.com/lambdasistemi/gh-dashboard/blob/main/src/Main.purs#L321-L326)

### Refresh Logic

- **doRefresh**: If repoList empty, seeds from API (first 25 repos). Otherwise fetches each repo individually to update.
  → [Refresh.purs:30-72](https://github.com/lambdasistemi/gh-dashboard/blob/main/src/Refresh.purs#L30-L72)

- **refreshSinglePR**: Fetches PR data + check runs + commit statuses, updates state.
  → [Refresh.purs:75-129](https://github.com/lambdasistemi/gh-dashboard/blob/main/src/Refresh.purs#L75-L129)

### UI Interactions

- **ToggleExpand**: Expand/collapse repo detail panel. Clears old detail when switching repos.
  → [Main.purs:327-346](https://github.com/lambdasistemi/gh-dashboard/blob/main/src/Main.purs#L327-L346)

- **ToggleItem**: Expand/collapse issues or PRs accordion. Triggers fetch if section is empty.
  → [Main.purs:347-374](https://github.com/lambdasistemi/gh-dashboard/blob/main/src/Main.purs#L347-L374)

- **SetFilter**: Update search filter text.
  → [Main.purs:375-376](https://github.com/lambdasistemi/gh-dashboard/blob/main/src/Main.purs#L375-L376)

- **DragStart/DragDrop**: Drag-and-drop to reorder repos.
  → [Main.purs:377-394](https://github.com/lambdasistemi/gh-dashboard/blob/main/src/Main.purs#L377-L394)

- **ToggleAddRepo/SubmitAddRepo**: Show form and add new repo by URL.
  → [Main.purs:395-439](https://github.com/lambdasistemi/gh-dashboard/blob/main/src/Main.purs#L395-L439)

- **RemoveRepo**: Remove repo from list with confirmation.
  → [Main.purs:440-463](https://github.com/lambdasistemi/gh-dashboard/blob/main/src/Main.purs#L440-L463)

- **HideItem**: Toggle hide/show for individual issue or PR.
  → [Main.purs:464-472](https://github.com/lambdasistemi/gh-dashboard/blob/main/src/Main.purs#L464-L472)

- **CopyText**: Copy text to clipboard.
  → [Main.purs:473-474](https://github.com/lambdasistemi/gh-dashboard/blob/main/src/Main.purs#L473-L474)

- **ToggleIssueLabelFilter/TogglePRLabelFilter**: Toggle label filter.
  → [Main.purs:475-499](https://github.com/lambdasistemi/gh-dashboard/blob/main/src/Main.purs#L475-L499)

- **ToggleTheme**: Switch dark/light theme.
  → [Main.purs:500-506](https://github.com/lambdasistemi/gh-dashboard/blob/main/src/Main.purs#L500-L506)

- **ExportStorage/ImportStorage**: Download/upload settings backup.
  → [Main.purs:507-510](https://github.com/lambdasistemi/gh-dashboard/blob/main/src/Main.purs#L507-L510)

- **ResetAll**: Clear all data with confirmation.
  → [Main.purs:511-530](https://github.com/lambdasistemi/gh-dashboard/blob/main/src/Main.purs#L511-L530)

---

## 🎨 View Layer

Halogen HTML rendering functions.

### Top Level

- **render**: Shows token form if not authenticated, otherwise dashboard.
  → [Main.purs:100-106](https://github.com/lambdasistemi/gh-dashboard/blob/main/src/Main.purs#L100-L106)

- **renderTokenForm**: Token input with instructions.
  → [View.purs:21-84](https://github.com/lambdasistemi/gh-dashboard/blob/main/src/View.purs#L21-L84)

- **renderDashboard**: Toolbar + add repo bar + error + repo table.
  → [View.purs:87-125](https://github.com/lambdasistemi/gh-dashboard/blob/main/src/View.purs#L87-L125)

### Toolbar

Add button, filter input, rate limit display, theme toggle, GitHub link, export/import/reset buttons.

→ [View.purs:128-199](https://github.com/lambdasistemi/gh-dashboard/blob/main/src/View.purs#L128-L199)

### Repo Table

Renders list of repos with drag-drop support. Each row shows: name, language, visibility, issues count, action buttons.

→ [View/RepoTable.purs](https://github.com/lambdasistemi/gh-dashboard/blob/main/src/View/RepoTable.purs)

### Issue List

Accordion section for issues. Shows title, author, labels, assignees. Click to expand body.

→ [View/Issues.purs](https://github.com/lambdasistemi/gh-dashboard/blob/main/src/View/Issues.purs)

### PR List

Accordion section for PRs. Like issues but includes CI status badges (green/yellow/red based on check conclusions).

→ [View/PRs.purs](https://github.com/lambdasistemi/gh-dashboard/blob/main/src/View/PRs.purs)

### Detail Widgets

Shared components: label badges, assignee avatars, status icons.

→ [View/DetailWidgets.purs](https://github.com/lambdasistemi/gh-dashboard/blob/main/src/View/DetailWidgets.purs)

### Helpers

Utility functions: date formatting, URL parsing, CSS class helpers.

→ [View/Helpers.purs](https://github.com/lambdasistemi/gh-dashboard/blob/main/src/View/Helpers.purs)

---

## 💾 Persistence Layer

All user data persisted to browser localStorage.

### Storage Keys

| Key | Content |
|-----|---------|
| `gh-dashboard-token` | GitHub token (string) |
| `gh-dashboard-repos` | Ordered repo list (JSON array) |
| `gh-dashboard-hidden` | Hidden item URLs (JSON array) |
| `gh-dashboard-dark-theme` | Theme boolean |
| `gh-dashboard-issue-labels` | Issue label filters (JSON array) |
| `gh-dashboard-pr-labels` | PR label filters (JSON array) |

→ [Storage.purs:34-50](https://github.com/lambdasistemi/gh-dashboard/blob/main/src/Storage.purs#L34-L50)

### Operations

- **loadToken/saveToken**: String read/write.
  → [Storage.purs:52-62](https://github.com/lambdasistemi/gh-dashboard/blob/main/src/Storage.purs#L52-L62)

- **loadRepoList/saveRepoList**: JSON array of repo names.
  → [Storage.purs:64-85](https://github.com/lambdasistemi/gh-dashboard/blob/main/src/Storage.purs#L64-L85)

- **loadHidden/saveHidden**: Set of hidden URLs.
  → [Storage.purs:150-175](https://github.com/lambdasistemi/gh-dashboard/blob/main/src/Storage.purs#L150-L175)

- **loadTheme/saveTheme**: Boolean (default true = dark).
  → [Storage.purs:87-100](https://github.com/lambdasistemi/gh-dashboard/blob/main/src/Storage.purs#L87-L100)

- **loadIssueLabelFilters/loadPRLabelFilters**: Sets of label names.
  → [Storage.purs:102-112](https://github.com/lambdasistemi/gh-dashboard/blob/main/src/Storage.purs#L102-L112)

- **clearAll**: Removes all keys.
  → [Storage.purs:139-148](https://github.com/lambdasistemi/gh-dashboard/blob/main/src/Storage.purs#L139-L148)

---

## 🔌 FFI (JavaScript Interop)

PureScript Foreign Function Interface for browser APIs.

### Clipboard

Copy text to clipboard using navigator.clipboard API.

→ [FFI/Clipboard.js](https://github.com/lambdasistemi/gh-dashboard/blob/main/src/FFI/Clipboard.js) + [FFI/Clipboard.purs](https://github.com/lambdasistemi/gh-dashboard/blob/main/src/FFI/Clipboard.purs)

### Storage Export/Import

- **exportStorage**: Downloads all localStorage as JSON file.
- **importStorage**: File picker to restore from JSON backup.

→ [FFI/Storage.js](https://github.com/lambdasistemi/gh-dashboard/blob/main/src/FFI/Storage.js) + [FFI/Storage.purs](https://github.com/lambdasistemi/gh-dashboard/blob/main/src/FFI/Storage.purs)

### Theme

Sets `dark` or `light` class on document body for CSS theming.

→ [FFI/Theme.js](https://github.com/lambdasistemi/gh-dashboard/blob/main/src/FFI/Theme.js) + [FFI/Theme.purs](https://github.com/lambdasistemi/gh-dashboard/blob/main/src/FFI/Theme.purs)

---

## 🛠️ Utilities

### RepoUtils

Helper functions for repo list management.

- **parseRepoName**: Extracts `owner/repo` from GitHub URL.
- **applyFilter**: Filters repos by name substring.
- **orderRepos**: Sorts repos array to match repoList order.
- **upsertRepo**: Insert or update repo in array.
- **moveItem**: Reorder item in list (for drag-drop).

→ [RepoUtils.purs](https://github.com/lambdasistemi/gh-dashboard/blob/main/src/RepoUtils.purs)

---

## 📁 Project Structure

```
gh-dashboard/
├── src/
│   ├── Main.purs          # Entry point, Halogen component, action handlers
│   ├── Types.purs         # Domain types (Repo, Issue, PR, CheckRun)
│   ├── GitHub.purs        # API client with pagination
│   ├── Storage.purs       # localStorage helpers
│   ├── Refresh.purs       # Async refresh logic
│   ├── RepoUtils.purs     # List manipulation utilities
│   ├── View.purs          # Top-level render functions
│   ├── View/
│   │   ├── Types.purs     # State and Action types
│   │   ├── RepoTable.purs # Repo list with drag-drop
│   │   ├── Issues.purs    # Issue accordion
│   │   ├── PRs.purs       # PR accordion with CI status
│   │   ├── Detail.purs    # Expanded repo detail panel
│   │   ├── DetailWidgets.purs # Shared UI components
│   │   └── Helpers.purs   # Utility functions
│   └── FFI/
│       ├── Clipboard.*    # Copy to clipboard
│       ├── Storage.*      # Export/import
│       └── Theme.*        # Dark/light mode
├── dist/                  # Built assets
├── spago.yaml             # PureScript dependencies
├── flake.nix              # Nix development environment
└── justfile               # Build recipes
```
