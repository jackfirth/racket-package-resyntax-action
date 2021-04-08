#!/usr/bin/env bash
set -ev
cd "$GITHUB_WORKSPACE"
if [[ -z "$INPUT_NAME" ]]; then
    INPUT_NAME=$(basename "$GITHUB_REPOSITORY")
fi
INPUT_DIRECTORY=$(realpath "$PWD/$INPUT_DIRECTORY")

git fetch --depth=1 origin "$GITHUB_BASE_REF"

find .git/refs

git status

git branch -v --all

env

xvfb-run -a -e /dev/stdout raco pkg install \
    --name "$INPUT_NAME" \
    --batch \
    --auto \
    --link \
    --scope installation \
    --skip-installed \
    "$INPUT_DIRECTORY"

xvfb-run racket -l racket-package-resyntax-action
