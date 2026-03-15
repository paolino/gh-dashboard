build:
    spago build

bundle:
    spago bundle

dev:
    spago build --watch

format:
    purs-tidy format-in-place src/**/*.purs

lint:
    purs-tidy check src/**/*.purs

ci: lint build bundle test-run

serve: bundle
    npx serve dist -p 10001

restart: bundle
    -pkill -f 'serve dist -p 10001'
    sleep 1
    npx serve dist -p 10001

test-run:
    npx playwright test

test: bundle test-run

test-auth: bundle
    GH_DASHBOARD_TOKEN=$(gh auth token) npx playwright test

clean:
    rm -rf output/ dist/index.js
