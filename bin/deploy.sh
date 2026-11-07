#!/usr/bin/env bash
set -euo pipefail

: "${DEPLOY_HOST:?Set DEPLOY_HOST to your SSH destination}"
: "${DEPLOY_PATH:?Set DEPLOY_PATH to an absolute server directory}"

# Only safe path characters are accepted because the path enters a remote shell.
if [[ ! "$DEPLOY_PATH" =~ ^/[a-zA-Z0-9_./-]+$ ]]; then
  echo "DEPLOY_PATH must be an absolute path without spaces or shell metacharacters." >&2
  exit 1
fi

release_name="release-$(date +'%Y%m%d_%H%M%S').tar"
npm run build
tar cf "$release_name" build
scp "$release_name" "$DEPLOY_HOST:~/"
ssh "$DEPLOY_HOST" "set -eu; mv ~/$release_name $DEPLOY_PATH/$release_name; cd $DEPLOY_PATH; tar xf $release_name; if [ -e current.last ]; then echo 'current.last exists; archive it before deploying' >&2; exit 1; fi; if [ -e current ]; then mv current current.last; fi; mv build current;"
echo "FINISHED."
