# gh-dashboard

[![CI](https://github.com/paolino/gh-dashboard/actions/workflows/ci.yaml/badge.svg)](https://github.com/paolino/gh-dashboard/actions/workflows/ci.yaml)

**[Live demo](https://paolino.github.io/gh-dashboard/)**

Repository dashboard for GitHub — browse your repos, open issues and pull requests on a single page with granular refresh controls, expandable detail panels and inline markdown rendering.

**Runs entirely in your browser.** There is no server — the app is a static page hosted on GitHub Pages. Your GitHub token is stored in localStorage and sent only to the GitHub API. Nothing is logged or transmitted to any third party.

## Features

- **Repo list** — first connection seeds 25 most recently updated repos; order persists in localStorage
- **Drag-and-drop reordering** — grab the ☰ handle to rearrange repos
- **Add / remove repos** — `+` button to add any public or private repo by URL; trash button to remove
- **Expandable detail panels** — click a repo row to reveal its issues and pull requests sections
- **Granular refresh** — ↻ buttons at every level: section headings reload all issues or PRs; per-row buttons refresh a single issue or PR (including CI checks)
- **Progressive PR loading** — PRs appear one at a time as their CI check runs are fetched
- **Collapsible sections** — Issues and PRs sections expand/collapse independently; expanding an empty section auto-triggers a refresh
- **Hide / unhide items** — hide individual issues or PRs; hidden items are grouped in a collapsible "Hidden" section within each group, persisted across sessions
- **Label filtering** — clickable label selector per section (issues and PRs independently); multi-select with OR logic; persisted in localStorage
- **CI status filtering** — PR check run statuses (success, failure, pending) appear as filterable labels alongside regular labels
- **Inline markdown** — click an issue or PR to render its body as markdown
- **Copy to clipboard** — copy issue/PR titles with one click
- **Filter** — live text filter across repo names and descriptions
- **Dark / light theme** — toggle between dark and light themes; persisted in localStorage
- **Tooltips** — hover any icon for a description of its action
- **Rate limit display** — shows remaining GitHub API quota
- **Reset** — clears all saved data (token, repo list, hidden items, theme, filters)

## Stack

PureScript · Halogen · GitHub REST API · marked.js · esbuild

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
just clean    # remove build artifacts
```

Open `dist/index.html` in a browser after bundling.

## License

MIT
