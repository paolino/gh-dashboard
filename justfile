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

ci: lint build bundle

serve: bundle
    npx serve dist -p 10001

restart: bundle
    -pkill -f 'serve dist -p 10001'
    sleep 1
    npx serve dist -p 10001

clean:
    rm -rf output/ dist/index.js
