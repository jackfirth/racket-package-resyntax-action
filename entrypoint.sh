#!/usr/bin/env bash

set -euo pipefail

cd "$GITHUB_WORKSPACE"

# Absolutely no idea why GitHub Actions seems convinced this package isn't
# installed already by this point, but whatever.
raco pkg install --batch --auto https://github.com/jackfirth/resyntax.git

if [ "$INPUT_DIRECTORY" == "." ]; then
  # As of August 2019, installing a linked package with source directory "."
  # raises an error for some bizarre reason, even when the package's name is
  # specified explicitly with --name. This is especially strange because source
  # directories like "./foo" work just fine.
  raco pkg install --name "$INPUT_NAME" --batch --auto --link
else
  raco pkg install --name "$INPUT_NAME" --batch --auto --link "$INPUT_DIRECTORY"
fi

racket -l resyntax/github-action "$INPUT_NAME"
