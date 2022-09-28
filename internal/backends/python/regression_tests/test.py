#!/usr/bin/env python3

import json
import subprocess
import sys

TEST_DIR = "test"

def load_json_file(filepath):
    file = open(filepath)
    data = json.load(file)
    file.close()
    return data

def reverse_mapping(dct):
    retval = {}
    for key, value in dct.items():
        value = value.lower()
        if value in retval:
            retval[value].append(key)
        else:
            retval[value] = [key]
    
    return retval

module_to_pypi = load_json_file("../module_to_pypi.json")
pypi_to_module = reverse_mapping(module_to_pypi)

skip_manual_checked = {
    'python-louvain': True,
    'pymilvus': True,
    'transformers': True,
    'configparser': True,
    'spark-nlp': True,
    'aws-sam-cli': True,
    'suds-jurko': True,
    'opsgenie-sdk': True,
    'dbutils': True,
    'biopython': True,
    'cmake-format': True,
    'mbed-tools': True,
    'fake-factory': True,
    'pysqlite3': True,
    'paddle2onnx': True,
    'django-extra-fields': True,
    'pyicu': True,
    'censys': True,
    'sip': True,
    'giturlparse': True,
    'filesplit': True,
    'pycausalimpact': True,
    'pip-autoremove': True,
    'demisto-py': True,
    'enos-mqtt-sdk-python': True,
    'cyclonedx-bom': True,
    'weather-api': True,
    'bio': True,
    'express': True,
    'django-meta': True,
    'frappe-bench': True,
    'python-chess': True,
    'optimuspyspark': True,
    'poetry': True,
    'pymatgen': True,
    'seeq': True,
    'pystan': True,
}

# override the module choice from the module_to_pypi.json file
override = {
    'tableau-api-lib': 'tableau_api_lib',
    'wmd': 'wmd',
    "django-tinymce": "tinymce",
    "cmake-format": "cmakelang",
    "pytest-xprocess": "xprocess",
    "notifications-python-client": "notifications_python_client",
    "imdb-cli-tool": "imdb_cli_tool",
}

def test_package(pkg):
    if pkg in skip_manual_checked:
        print("skip %s" % pkg)
        return
    main_file = open(TEST_DIR + "/main.py", "w")
    if pkg not in pypi_to_module:
        print("%s is not in the mapping" % pkg)
        return
    if pkg in override:
        mod = override[pkg]
    else:
        mod = choose_module(pkg, pypi_to_module[pkg])
    main_file.write("import %s" % mod)
    main_file.close()

    proc1 = subprocess.run(["upm-old", "guess", "-f"], cwd=TEST_DIR, capture_output=True)
    proc2 = subprocess.run(["upm", "guess", "-f"], cwd=TEST_DIR, capture_output=True)
    if proc1.stdout == proc2.stdout:
        print("%s ok" % pkg)
    else:
        print("%s failed" % pkg)
        print("  Expected: %s" % proc1.stdout)
        print("  Actual: %s" % proc2.stdout)

def choose_module(pkg, modules):
    best_score = 0
    choosen = None
    for mod in modules:
        if mod == pkg:
            return mod
        if mod.startswith("_"):
            score = 1
        else:
            score = 2
        if score > best_score:
            best_score = score
            choosen = mod
    return choosen

if len(sys.argv) > 1:
    pkg = sys.argv[1]
    test_package(pkg)
else:
    top_10k = load_json_file("../top_10k.json")
    for pkg in top_10k:
        test_package(pkg)



