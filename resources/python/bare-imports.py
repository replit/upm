# This is a Python script that implements bare imports for Python
# using pipreqs. It takes no arguments, and dumps a list of package
# names (strings) to stdout in JSON format. The script works in both
# Python 2 and Python 3. It expects pipreqs.py to be on the
# PYTHONPATH. UPM accomplishes this by writing it into the same
# directory as this script.

from __future__ import print_function
import json
import pipreqs
import sys

imports, had_errors = pipreqs.get_all_imports(
    ".", extra_ignore_dirs=sys.argv[1].split()
)
json.dump({"imports": imports, "success": not had_errors}, sys.stdout)
