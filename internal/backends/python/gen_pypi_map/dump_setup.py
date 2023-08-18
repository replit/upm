# Everything is in a try catch to ensure no errors sneak past the caller
try:
    import json
    import os
    import runpy
    import sys

    metadata = None

    def setup_callback(*args, **kwargs):
        global metadata
        if metadata is None:
            metadata = kwargs
        else:
            raise Exception("Setup called more then once")

    # Monkeypatch setup functions to use our callback
    try:
        import setuptools
        setuptools.setup = setup_callback
    except ModuleNotFoundError:
        pass

    try:
        import distutils.core
        distutils.core.setup = setup_callback
    except ModuleNotFoundError:
        pass

    # Locate setup.py
    setup_py = os.path.abspath(sys.argv[1])
    working_dir = os.path.dirname(setup_py)
    os.chdir(working_dir)

    # Redirect stdout to stderr for the setup scipt so we don't pollute stdout
    stdout = sys.stdout
    sys.stdout = sys.stderr

    # Run setup.py
    runpy.run_path(setup_py, run_name="__main__")

    # Put stdout back so we can report metadata
    sys.stdout = stdout

    # Make sure setup was called once
    if metadata is None:
        raise Exception("Setup was never called")

    # Report metadata to caller
    print(json.dumps(metadata, default=lambda o: repr(o)))

except Exception as e:
    # If any errors occur, serialize and report
    print(json.dumps({'error': str(e)}))
