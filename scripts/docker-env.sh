#!/bin/sh

export PATH="/upm/cmd/upm:$PATH"
cd /upm
alias u='(cd /upm && make -s upm) && upm'
