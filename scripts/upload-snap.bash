#!/usr/bin/env bash

set -e
set -o pipefail

tag_name="v$(ls -1 dist/upm_*.tar.gz | grep -Eo '[0-9.]+' | head -n1)"

release_id="$(curl -s https://api.github.com/repos/replit/upm/releases \
                   -H "Authorization: token $GITHUB_TOKEN" | jq "map(select(.tag_name == \"$tag_name\")) | .[0].id")"

if [[ -z "$release_id" ]]; then
    echo "no release id" >&2
    exit 1
fi

for file in dist/*.snap; do
    curl "https://uploads.github.com/repos/replit/upm/releases/${release_id}/assets?name=$(basename "$file")" \
         --silent \
         --data-binary @"${file}" \
         -H "Authorization: token ${GITHUB_TOKEN}" \
         -H "Content-Type: application/octet-stream" | jq
done
