#!/bin/sh
pulp \
  --watch \
  --before clear \
  --then "say holy crow && $(dirname $0)/build.sh" \
  --else "say it doesn\'t like it" \
  build \
  --jobs 2 \
  -- --stash

--then "$(dirname $0)/build.sh && say holy crow" \
