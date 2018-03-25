#!/bin/sh
set -e
#et -x

OUT=dist

bower install

mkdir -p $OUT
rm -f $OUT/*.css
rm -f $OUT/*html
rm -rf $OUT/img

cp -R resources/img $OUT
cp -R resources/css/* $OUT
cp -R resources/html/* $OUT
cp resources/manifest.json $OUT

if [ ! -e $OUT/popup.js ] || [ $(find src -newer $OUT/popup.js -name '*.purs' | wc -l) -gt 0 ]; then
  pulp build --jobs 2 --main Pinboard.UI.Popup --to $OUT/popup.js #2>/dev/null
fi

# if [ ! -e $OUT/background.js ] || [ $(find src -newer $OUT/background.js -name '*.purs' | wc -l) -gt 0 ]; then
#   pulp build --jobs 2 --main Pinboard.UI.Background --to $OUT/background.js #2>/dev/null
# fi
