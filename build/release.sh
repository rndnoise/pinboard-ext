#!/bin/sh
set -e

BOLD=$(tput bold)
NORM=$(tput sgr0)

abort() {
  echo $1 >&2
  exit 1
}

[ $# -eq 1 ] || abort "usage: $0 <VERSION>"

VERSION=$1

command -v git      >/dev/null 2>&1 || abort "git not in $PATH"
command -v jq       >/dev/null 2>&1 || abort "jq not in $PATH"
command -v web-ext  >/dev/null 2>&1 || abort "web-ext not in $PATH"
command -v zip      >/dev/null 2>&1 || abort "zip not in $PATH"
command -v openssl  >/dev/null 2>&1 || abort "openssl not in $PATH"

git rev-parse "$VERSION" >/dev/null 2>&1 \
  && abort "tag already exists"

mkdir -p $(basename $0)/srcs
mkdir -p $(basename $0)/zips

X=$(git status --porcelain) && [ -z "$X" ]  || abort "aborting due to unstaged changes"
$(dirname $0)/build.sh                      || abort "build failed"

echo "${BOLD}validating manifest.json${NORM}"
for X in $(jq -r ".icons | .[]" < resources/manifest.json); do
  [ -f build/srcs/$X ] || abort "icon doesn't exist: $X"
done
for X in $(jq -r ".browser_action | .default_icon | .[]" < resources/manifest.json); do
  [ -f build/srcs/$X ] || abort "icon doesn't exist: $X"
done

echo "${BOLD}updating manifest.json${NORM}"
jq ".version = "'"'${VERSION}'"' < resources/manifest.json > resources/manifest.json_
mv resources/manifest.json_ resources/manifest.json
cp resources/manifest.json $(dirname $0)/srcs

echo "${BOLD}signing firefox package${NORM}"
web-ext sign \
  --api-key="user:13897535:719" \
  --api-secret="8ad53a40955965b21520b13c81dbea1d7b6b533f053f56ee6b8d0ceb4d2d506c" \
  --id "{63b5b423-d5bc-4030-80fc-ea8a55067532}" \
  --source-dir $(dirname $0)/srcs \
  --artifacts-dir $(dirname $0)/zips

echo "${BOLD}tagging release${NORM}"
git add resources/manifest.json     >/dev/null
git commit -m "release v${VERSION}" >/dev/null
git tag ${VERSION}                  >/dev/null

echo "${BOLD}copying .xpi firefox packages to website${NORM}"
OUT=~/wd/write/randomnoi.se/assets/files/pinboard
cp $(dirname $0)/zips/*xpi ${OUT}

echo "${BOLD}updating firefox.json${NORM}"
$(dirname $0)/firefox-updates.sh > ${OUT}/firefox.json
ln -sf $(basename ${OUT}/*${VERSION}[^0-9.]*.xpi) ${OUT}/pinboard-current.xpi

echo "packaging ${BOLD}build/zips/pinboard-$VERSION.zip${NORM}"
zip -j $(dirname $0)/zips/pinboard-${VERSION}.zip $(dirname $0)/srcs/* $(dirname $0)/srcs/*/*

echo
echo "${BOLD}don't forget${NORM}"
echo "  https://addons.opera.com/developer/upload/"
echo "  https://chrome.google.com/webstore/developer/dashboard"
echo "  deploy randomnoi.se"
