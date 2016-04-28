#!/bin/bash

set -o errexit

cd _build/doc

rm -rf .git
git init
git config user.name "Vincent Jacques (on Travis-CI)"
git config user.email "vincent@vincent-jacques.net"
touch .nojekyll
git add .
git commit -m "Generate documentation"

if [ -n "$GITHUB_TOKEN" ]
then
    git push -f https://$GITHUB_TOKEN:@github.com/jacquev6/hashids-ocaml.git master:gh-pages 2>&1 \
    | sed s/$GITHUB_TOKEN/GITHUB_TOKEN/g
else
    git push -f git@github.com:jacquev6/hashids-ocaml.git master:gh-pages
fi
