#!/usr/bin/env bash

set -ev

cd "$GITHUB_WORKSPACE"

# Absolutely no idea why GitHub Actions seems convinced this package isn't
# installed already by this point, but whatever.
# raco pkg install --batch --auto https://github.com/jackfirth/resyntax.git

if [[ -z "$INPUT_NAME" ]]; then
    INPUT_NAME=$(basename "$GITHUB_REPOSITORY")
fi

raco pkg install --name "$INPUT_NAME" --batch --auto --link "$INPUT_DIRECTORY"
racket -l racket-package-resyntax-action
