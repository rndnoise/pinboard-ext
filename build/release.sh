#!/bin/sh
set -e

abort() {
  echo $1 >&2
  exit 1
}

[ $# -eq 1 ] \
  || abort "usage: $0 <VERSION>"

x=$(git status --porcelain) && [ -z "$x" ] \
  || abort "unstaged changes!"

$(dirname $0)/build.sh \
  || abort "build failed"

# check that each icon exists
for x in $(jq -r ".icons | .[]" < resources/manifest.json); do
  [ ! -f dist/$x ] || abort "icon doesn't exist: $x"
done

# check that each icon exists
for x in $(jq -r ".browser_action | .default_icon | .[]" < resources/manifest.json); do
  [ ! -f dist/$x ] || abort "icon doesn't exist: $x"
done

echo "setting version in resources/manifest.json"
set -x
jq ".version = "'"'$1'"' < resources/manifest.json > resources/manifest.json_
mv resources/manifest.json_ resources/manifest.json
cp resources/manifest.json $(dirname $0)/build-srcs

web-ext sign \
  --api-key="user:13897535:719" \
  --api-secret="8ad53a40955965b21520b13c81dbea1d7b6b533f053f56ee6b8d0ceb4d2d506c" \
  --id "{63b5b423-d5bc-4030-80fc-ea8a55067532}" \
  --source-dir $(dirname $0)/dist-srcs \
  --artifacts-dir $(dirname $0)/dist-zips

git add resources/manifest.json
git commit -m "release v$VERSION"
git tag $VERSION

set +x
echo "copying .xpi firefox packages to website"
OUT=~/wd/write/randomnoi.se/resources/files/pinboard

$(dirname $0)/firefox-updates.sh > $OUT/firefox.json
cp -v dist-zips/*xpi $OUT

echo "don't forget"
echo "  https://addons.opera.com/developer/upload/"
echo "  https://chrome.google.com/webstore/developer/dashboard"
