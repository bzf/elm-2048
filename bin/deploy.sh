#!/bin/sh

COMMIT_HASH=`git log | head -n 1 | cut -c 8-14`

git config name $GIT_NAME
git config user.email $GIT_EMAIL

echo "Deploying commit $COMMIT_HASH as $GIT_NAME"

git add dist/ && git commit -m "Built commit $COMMIT_HASH"
git subtree push --prefix dist origin gh-pages
