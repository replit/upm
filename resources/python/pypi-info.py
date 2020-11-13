# This is a Python script that looks up package metadata on PyPI.
# It takes one argument, the name of the package (not
# necessarily canonical), and outputs the results in JSON format (a
# map). The script works on both Python 2 and Python 3.

from __future__ import print_function
import requests
import json
import sys

if len(sys.argv) != 2:
    print("Invalid number of arguments. Usage: python pypy-info.py pkg")
    sys.exit(1)

package = sys.argv[1]

result = requests.get("https://pypi.org/pypi/%s/json"% package)
if result.status_code != 200:
    print("{}")
    sys.exit(0)

info = result.json()['info']
json.dump(info, sys.stdout)
