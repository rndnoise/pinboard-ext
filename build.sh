#!/bin/sh
set -e
set -x

bower install

mkdir -p dist
rm -f dist/*.css
rm -f dist/*html
rm -rf dist/img

cp -R resources/img dist
cp -R resources/css/* dist
cp -R resources/html/* dist
cp resources/manifest.json dist

if [ ! -e dist/popup.js ] || [ $(find src -newer dist/popup.js -name '*.purs' | wc -l) -gt 0 ]; then
  pulp build --jobs 2 --main Pinboard.UI.Popup --to dist/popup.js #2>/dev/null
fi

if [ ! -e dist/options.js ] || [ $(find src -newer dist/options.js -name '*.purs' | wc -l) -gt 0 ]; then
  pulp build --jobs 2 --main Pinboard.UI.Options --to dist/options.js #2>/dev/null
fi

if [ ! -e dist/background.js ] || [ $(find src -newer dist/background.js -name '*.purs' | wc -l) -gt 0 ]; then
  pulp build --jobs 2 --main Pinboard.UI.Background --to dist/background.js #2>/dev/null
fi
