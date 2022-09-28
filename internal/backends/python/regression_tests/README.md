# Regression Tests

These tests were put in place to make sure we didn't break the guessing
behavior after a big rewrite.

This test suite tests the new `upm` against the old version of upm: `upm-old`.

## Setup

To set up the test, first you'll need to install the old version of upm by checking out an older version. At the top of the project directory, do:

```
git checkout abe5cc5
rm internal/backends/python/pypi_map.gen.go
make upm
go install ./cmd/upm
cp $(which upm) $(dirname $(which upm))/upm-old
```

Now comeback to the newer branch:

```
git checkout th-python-guess-modules-2
make clean-gen
make upm
make install
```

Now you should have access to both `upm` and `upm-old`.

## Run a test

cd into the `regression_tests` directory.

To test a specific module, pass its name to the test.py script:

```
python3 test.py flask
```

You should see output like `flask ok` if the output of the 2 binary were the same.
If you old upm didn't have the package in its mapping, it will say:
`<pkg> is not in the mapping`. If the output do not match between the 2, it will
say `<pkg> failed`.

To run the regression tests on the top 10k packages, you can leave out the package name:

```
python3 test.py
```

This can take up to 30 minutes depending on hardware.



