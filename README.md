# gh-dashboard

[![CI](https://github.com/lambdasistemi/gh-dashboard/actions/workflows/ci.yaml/badge.svg)](https://github.com/lambdasistemi/gh-dashboard/actions/workflows/ci.yaml)

**[Live demo](https://lambdasistemi.github.io/gh-dashboard/)**

Client-side GitHub dashboard — browse repos, issues, pull requests and projects on a single page. Runs entirely in the browser as a static page on GitHub Pages. Your token stays in localStorage and is sent only to the GitHub API.

## Features

### Repositories

- First connection seeds the 25 most recently updated repos; order persists in localStorage
- Drag-and-drop reordering via ☰ handle
- Add any public or private repo by URL; remove with the trash button
- Live text filter across repo names and descriptions

### Issues & Pull Requests

- Expandable detail panels per repo with independent Issues and PRs sections
- Granular refresh — ↻ at section level reloads all; per-row buttons refresh a single item (including CI checks)
- Progressive PR loading — PRs appear one at a time as CI checks are fetched
- Collapsible sections — expanding an empty section auto-triggers a refresh
- Hide / unhide items — hidden items grouped in a collapsible "Hidden" section, persisted across sessions
- Label filtering — multi-select with OR logic, persisted in localStorage
- CI status filtering — check run statuses (success, failure, pending) as filterable labels
- Inline markdown rendering for issue and PR bodies
- Copy titles to clipboard

### GitHub Projects

- Dedicated Projects tab with read/write support
- Add, update and delete project items
- Inline rename for projects
- Status management (Todo, In Progress, Done, Backlog)
- Copy project item titles to clipboard

### Agent Daemon

- Launch, detach and stop [agent-daemon](https://github.com/lambdasistemi/agent-daemon) sessions from issue and project item rows
- Inline xterm.js terminal replaces the description body with a live WebSocket-connected terminal
- Resizable terminals — drag the handle below each terminal to adjust height
- Active terminal highlighting — rows with a running terminal get a blue accent border

### UI

- Dark / light theme toggle, persisted in localStorage
- Tooltips on all action icons
- Rate limit display showing remaining GitHub API quota
- Import / export settings as JSON (token excluded for security)
- Reset button clears all saved data
- Responsive design — mobile-friendly layout

## Token scopes

Create a [personal access token](https://github.com/settings/tokens/new?scopes=repo,read:project&description=gh-dashboard) with these scopes:

| Scope | Required for |
|-------|-------------|
| `repo` | Repos, issues, PRs, CI checks |
| `read:project` | Projects tab |

## Stack

PureScript · Halogen · GitHub REST & GraphQL API · marked.js · xterm.js · esbuild · Nix

## Development

Enter the Nix devShell:

```bash
nix develop
```

Available commands:

```bash
just build    # compile PureScript
just bundle   # produce dist/index.js
just dev      # watch mode
just format   # format sources with purs-tidy
just lint     # check formatting
just ci       # lint + build + bundle
just serve    # bundle and serve on port 10001
just restart  # bundle, kill old server, serve
just clean    # remove build artifacts
```

Open `dist/index.html` in a browser after bundling.

## License

MIT
