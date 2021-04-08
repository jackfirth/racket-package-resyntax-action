#!/usr/bin/env bash
set -ev
cd "$GITHUB_WORKSPACE"
if [[ -z "$INPUT_NAME" ]]; then
    INPUT_NAME=$(basename "$GITHUB_REPOSITORY")
    echo "INPUT_NAME is now $INPUT_NAME"
fi
echo "INPUT_NAME is now(2) $INPUT_NAME"

raco pkg install --name "$INPUT_NAME" --batch --auto --link "$INPUT_DIRECTORY"
racket -l racket-package-resyntax-action
