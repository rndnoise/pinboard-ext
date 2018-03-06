#!/bin/sh
pulp \
  --watch \
  --before clear \
  --then "say holy crow && ./build.sh" \
  --else "say it doesn\'t like it" \
  build \
  --jobs 2 \
  -- --stash

  --then "./build.sh && say holy crow" \
