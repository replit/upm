#!/usr/bin/env bash

set -e
set -o pipefail

info() {
    # Since this script is run in the background from the Makefile,
    # this apparently messes up the shell's ability to move the cursor
    # back to the leftmost column when a newline is printed. The
    # problem is fixed if we manually print a carriage return after
    # the message.
    echo "$@"$'\r' >&2
}

# Each loop iteration sleeps for 100ms, so this means an overall
# timeout of 5s.
for i in $(seq 50); do
    if curl -s localhost:6060 >/dev/null; then
        # Give some extra time for godoc to complete the initial scan.
        sleep 0.2
        url="http://localhost:6060/pkg/github.com/replit/upm/?m=all"
        if command -v xdg-open &>/dev/null; then
            info "godoc started; opening $url with xdg-open(1)"
            xdg-open "$url"
        elif command -v open &>/dev/null; then
            info "godoc started; opening $url with open(1)"
            open "$url"
        else
            info "please install either open(1) or xdg-open(1)"
            exit 1
        fi
        exit 1
    fi
    sleep 0.1
done

info "godoc failed to start listening on port 6060"
exit 1
