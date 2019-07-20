# This is a Python script that does a PyPI search using the XMLRPC
# API. It takes one argument, the search query (which may contain
# spaces), and outputs the results in JSON format (a list of
# pypiXMLRPCEntry maps). The script works on both Python 2 and Python
# 3.

from __future__ import print_function
import json
import sys

try:
    from xmlrpc import client as xmlrpc
except ImportError:
    import xmlrpclib as xmlrpc

query = sys.argv[1]
pypi = xmlrpc.ServerProxy("https://pypi.org/pypi")
results = pypi.search({"name": query})
json.dump(results, sys.stdout)
print()
