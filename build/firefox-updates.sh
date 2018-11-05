#!/bin/sh
set -e

read -d '' JQ << EOF || true
.addons[].updates[0] | . |=
  ( (.version |= sub("<VERSION>";       \$VERSION))
  | (.update_link |= sub("<BASENAME>";  \$BASENAME))
  | (.update_hash |= sub("<SHA256>";    \$SHA256))
  )
EOF

DIR=$(dirname $0)
OUT=$(mktemp)

for FILEPATH in $(ls -tr $DIR/zips/*xpi); do
  BASENAME=$(basename $FILEPATH)
  VERSION=$(echo $BASENAME | perl -ne '/([\d.]+)/; print $1')
  SHA256=$(openssl dgst -sha256 $FILEPATH | cut -d' ' -f2)

  cat resources/firefox.json | jq \
    --arg VERSION "$VERSION" \
    --arg SHA256 "$SHA256" \
    --arg BASENAME "$BASENAME" "$JQ"
done > $OUT

cat resources/firefox.json | jq --slurpfile UPDATES $OUT '.addons[].updates |= $UPDATES'
