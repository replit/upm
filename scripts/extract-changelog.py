#!/usr/bin/env python3

lines = []
found_section = False

with open("CHANGELOG.md") as f:
    for line in f:
        if line.startswith("## "):
            if found_section:
                break
            else:
                found_section = True
        elif found_section:
            lines.append(line)

while lines and lines[-1].isspace():
    lines.pop()

print("## Changes in this release\n\n", end="")
for line in lines:
    print(line, end="")
