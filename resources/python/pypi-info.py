# This is a Python script that looks up package metadata on PyPI using
# the XMLRPC API. It takes one argument, the name of the package (not
# necessarily canonical), and outputs the results in JSON format (a
# map, see pypiXMLRPCInfo). The script works on both Python 2 and
# Python 3.

from __future__ import print_function
import json
import sys

try:
    from xmlrpc import client as xmlrpc
except ImportError:
    import xmlrpclib as xmlrpc

package = sys.argv[1]
pypi = xmlrpc.ServerProxy("https://pypi.org/pypi")
releases = pypi.package_releases(package)
if not releases:
    print("{}")
    sys.exit(0)
release, = releases
info = pypi.release_data(package, release)
json.dump(info, sys.stdout)
