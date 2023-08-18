import json

try:
    import pkgutil
    import subprocess
    import sys

    package = sys.argv[1]

    # Determine the modules already installed
    pre_modules = set([mod_info.name for mod_info in pkgutil.iter_modules()])

    # Install the module under test
    subprocess.check_call([sys.executable, "-m", "pip", "install", package], stdout=subprocess.DEVNULL)

    # Find the new modules
    post_modules = [mod_info.name for mod_info in pkgutil.iter_modules()
                    if mod_info.name not in pre_modules]

    print(json.dumps({"modules": post_modules}))
except Exception as e:
    print(json.dumps({"error": str(e)}))
