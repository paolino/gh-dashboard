# gh-dashboard

**[Live demo](https://paolino.github.io/gh-dashboard/)**

Repository dashboard for GitHub — browse your repos, open issues and pull requests on a single page with auto-refresh, expandable detail panels and inline markdown rendering.

**Runs entirely in your browser.** There is no server — the app is a static page hosted on GitHub Pages. Your GitHub token is stored in localStorage and sent only to the GitHub API. Nothing is logged or transmitted to any third party.

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
