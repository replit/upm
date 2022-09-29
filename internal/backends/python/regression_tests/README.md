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

You should see 4 kinds of outputs:

* <pkg> ok - means the output of the old matches the new (or we added a manual override in test.py)
* <pkg> failed - means the output of the old does not match the new
* <pkg> added - means the old one did not have an entry for this pkg and the new one does
* <pkg> no-guess - means the old doesn't have an entry and the new couldn't find a guess (based on its heuistic algorithm)
* <pkg> test-errored - means the old does not have an entry for this pkg and the new one error during the package test
* <pkg> missing - means neither the old or the new has an entry for this pkg

To run the regression tests on the top 10k packages, you can leave out the package name:

```
python3 test.py
```

This can take up to 30 minutes depending on hardware.



