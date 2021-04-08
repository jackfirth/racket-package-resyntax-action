#!/usr/bin/env bash
set -ev
cd "$GITHUB_WORKSPACE"
if [[ -z "$INPUT_NAME" ]]; then
    INPUT_NAME=$(basename "$GITHUB_REPOSITORY")
fi
INPUT_DIRECTORY=$(realpath "$PWD/$INPUT_DIRECTORY")

raco pkg install --name "$INPUT_NAME" --batch --auto --link "$INPUT_DIRECTORY"
racket -l racket-package-resyntax-action
