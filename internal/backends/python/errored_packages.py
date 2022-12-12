import os
import json
f = open('pypi_packages.json')
mapping = {}
for line in f:
    info = json.loads(line)
    mapping[info['p'].lower()] = True
f.close()
entries = os.listdir("cache")
for entry in entries:
    name, _ = entry.split(".")
    f = open("cache/" + entry)
    info = json.load(f)
    f.close()
    name = name.lower()
    if 'error' in info and name not in mapping:
        print(name)
        