#!/usr/bin/env bash

# upgrade-tests.sh: Use each tool for each test suite to update lockfiles
#
# This is intended to prevent test suites from getting too far out of date and
# flagging in dependabot.

REPO_ROOT_DIR="$(git rev-parse --show-toplevel)"

set -ex

for dir in "${REPO_ROOT_DIR}/test-suite/templates/bun"/*/; do
  (cd "$dir"; bun update)
done

for dir in "${REPO_ROOT_DIR}/test-suite/templates/nodejs-npm"/*/; do
  (cd "$dir"; npm install)
done

for dir in "${REPO_ROOT_DIR}/test-suite/templates/nodejs-pnpm"/*/; do
  (cd "$dir"; pnpm update)
done

for dir in "${REPO_ROOT_DIR}/test-suite/templates/nodejs-yarn"/*/; do
  (cd "$dir"; yarn upgrade || yarn install)
done

for dir in "${REPO_ROOT_DIR}/test-suite/templates/python3-poetry"/*/; do
  (cd "$dir"; poetry update)
done

for dir in "${REPO_ROOT_DIR}/test-suite/templates/python3-uv"/*/; do
  (cd "$dir"; uv sync)
done
