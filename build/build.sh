#!/bin/sh
set -e

OUT=build/srcs

bower install

mkdir -p $OUT
mkdir -p $OUT/blue
rm -f $OUT/*.css
rm -f $OUT/*html
rm -f $OUT/*svg
rm -f $OUT/*png

cp -R resources/img/*.svg $OUT
cp -R resources/img/blue/*.png $OUT/blue
cp -R resources/css/* $OUT
cp -R resources/html/* $OUT
cp resources/manifest.json $OUT

if [ ! -e $OUT/popup.js ] || [ $(find src -newer $OUT/popup.js -name '*.purs' | wc -l) -gt 0 ]; then
  pulp build --jobs 2 -O --main Pinboard.UI.Popup --to $OUT/popup.js #2>/dev/null
fi

if [ ! -e $OUT/omnibox.js ] || [ $(find src -newer $OUT/omnibox.js -name '*.purs' | wc -l) -gt 0 ]; then
  pulp build --jobs 2 -O --main Pinboard.Background --to $OUT/omnibox.js #2>/dev/null
fi
