#!/usr/bin/env python
# -*- coding: utf-8 -*-

# From master branch of pipreqs post-0.4.7
# https://github.com/bndr/pipreqs/blob/15208540da03fdacf48fcb0a8b88b26da76b64f3/pipreqs/pipreqs.py.
#
# The get_all_imports function and supporting code have been pulled out along
# with a modification to the interface. get_all_imports changed so that it
# doesn't abort on errors, but rather returns a boolean to indicate whether
# there were any.

import os
import sys
import re
import codecs
import ast

if sys.version_info[0] > 2:
    open_func = open
    py2 = False
else:
    open_func = codecs.open
    py2 = True
    py2_exclude = ["concurrent", "concurrent.futures"]


def get_all_imports(
        path, encoding=None, extra_ignore_dirs=None, follow_links=True):
    imports = {}
    raw_imports = {}
    candidates = []
    ignore_dirs = [".hg", ".svn", ".git", ".tox", "__pycache__", "env", "venv"]

    if extra_ignore_dirs:
        ignore_dirs_parsed = []
        for e in extra_ignore_dirs:
            ignore_dirs_parsed.append(os.path.basename(os.path.realpath(e)))
        ignore_dirs.extend(ignore_dirs_parsed)

    had_errors = False
    walk = os.walk(path, followlinks=follow_links)
    for root, dirs, files in walk:
        dirs[:] = [d for d in dirs if d not in ignore_dirs]

        candidates.append(os.path.basename(root))
        files = [fn for fn in files if os.path.splitext(fn)[1] == ".py"]

        candidates += [os.path.splitext(fn)[0] for fn in files]
        for file_name in files:
            file_name = os.path.join(root, file_name)
            with open_func(file_name, "r", encoding=encoding) as f:
                contents = f.read()
            try:
                # We need to be able to reference a pragma in the comments
                lines = contents.split('\n')
                tree = ast.parse(contents)
                for node in ast.walk(tree):
                    modname = None
                    if isinstance(node, ast.Import):
                        for subnode in node.names:
                            modname = subnode.name
                    elif isinstance(node, ast.ImportFrom):
                        modname = node.module

                    # If the node was an import, look for pragmas
                    pragmas = {}
                    if modname:
                        # Which lines are part of this statement
                        statement_lines = lines[node.lineno - 1:
                                                node.end_lineno]

                        # Reconstruct the statement
                        line = ''.join([l.rstrip('\\')
                                        for l in statement_lines])

                        # If this line ends in a pragma add it
                        m = re.match('^.*#upm package\\((.*)\\).*$', line)
                        if m:
                            pragmas['package'] = m.group(1)

                        # Record the module name
                        # Name could have been None if the import
                        # statement was as ``from . import X``. We drop that
                        # case but including the insert in ``if modname``
                        raw_imports[modname] = pragmas
            except Exception as exc:
                had_errors = True
                continue

    imports = raw_imports
    missing_modules = imports.keys() - (set(candidates) & imports.keys())
    return {k: imports[k] for k in missing_modules}, had_errors

def join(f):
    return os.path.join(os.path.dirname(__file__), f)
