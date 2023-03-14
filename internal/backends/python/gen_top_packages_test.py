#!/usr/bin/env python3

import json
import sys

def getOutputFile():
	return sys.argv[len(sys.argv) - 1]

def getTopLimit():
	try:
		flag = sys.argv.index('-n')
		return int(sys.argv[flag + 1])
	except ValueError:
		return 10000

inputFile = open('download_stats.json')
stats = [(k, v) for k, v in json.load(inputFile).items()]
stats.sort(key=lambda item: item[1], reverse=True)
stats = stats[:getTopLimit()]
inputFile.close()

outputFile = open(getOutputFile(), 'w')
topPackages = [k for k, _ in stats]
json.dump(topPackages, outputFile, indent=2)
outputFile.close()
