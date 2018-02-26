#!/bin/sh
set -x
set -e

bower install

rm -rf dist
mkdir -p dist

cp -R resources/img dist
cp -R resources/css/* dist
cp -R resources/html/* dist
cp resources/manifest.json dist

pulp build --jobs 2 --optimise --main Pinboard.Popup --to dist/popup.js
pulp build --jobs 2 --optimise --main Pinboard.Options --to dist/options.js
pulp build --jobs 2 --optimise --main Pinboard.Background --to dist/background.js
