#! /usr/bin/env python

import json
import sys

f = open("download_stats.json")
stats = json.load(f)

entries = list(stats.items())
entries.sort(key=lambda e: e[1], reverse=True)

if len(sys.argv) < 2:
    print("Please provide a number")
    exit(1)


count = int(sys.argv[1])

result = list(map(lambda e: e[0], entries[:count]))

json.dump(result, sys.stdout, indent=2)
print()