# gh-dashboard

[![CI](https://github.com/paolino/gh-dashboard/actions/workflows/ci.yaml/badge.svg)](https://github.com/paolino/gh-dashboard/actions/workflows/ci.yaml)

**[Live demo](https://paolino.github.io/gh-dashboard/)**

Repository dashboard for GitHub — browse your repos, open issues and pull requests on a single page with auto-refresh, expandable detail panels and inline markdown rendering.

**Runs entirely in your browser.** There is no server — the app is a static page hosted on GitHub Pages. Your GitHub token is stored in localStorage and sent only to the GitHub API. Nothing is logged or transmitted to any third party.

## Features

- **Repo list** — first connection seeds 15 most recently updated repos; order persists in localStorage
- **Drag-and-drop reordering** — grab the ☰ handle to rearrange repos
- **Add / remove repos** — `+` button to add any public or private repo by URL; ✕ button to remove
- **Expandable detail panels** — click a repo row to load its open issues and pull requests
- **Collapsible sections** — Issues and PRs sections expand/collapse independently
- **Hide / unhide items** — hide individual issues or PRs; hidden items are grouped in a collapsible "Hidden" section within each group, persisted across sessions
- **Inline markdown** — click an issue or PR to render its body as markdown
- **Filter** — live text filter across repo names and descriptions
- **Auto-refresh** — configurable interval (default 60s), pause/resume toggle
- **Rate limit display** — shows remaining GitHub API quota
- **Reset** — clears all saved data (token, repo list, hidden items)

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
