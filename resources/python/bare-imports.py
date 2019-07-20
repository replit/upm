# This is a Python script that implements bare imports for Python
# using pipreqs. It takes no arguments, and dumps a list of package
# names (strings) to stdout in JSON format. The script works in both
# Python 2 and Python 3, but pipreqs has to be installed for the
# version of Python in use (e.g. you can't be inside a virtualenv;
# export UPM_PYTHON2 or UPM_PYTHON3 as appropriate if you are).

from __future__ import print_function
import json
import pipreqs.pipreqs as pipreqs
import sys

imports = pipreqs.get_all_imports(".", extra_ignore_dirs=sys.argv[1].split())
packages = pipreqs.get_pkg_names(imports)
json.dump(packages, sys.stdout)
