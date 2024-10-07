# gen_pypi_map

The gen_pypi_map module which generates the `pypi_map.gen.go` file, which
contains the
    1. module -> package mapping for guessing
    2. package -> modules mapping for detecting if a module is already installed
    3. package -> download count mapping for stats

It does this in separate steps, and gives a CLI interface for the admin
to walk through them. To run this program, it is recommended that you
have your working directory (CWD) set to `internal/backends/python`.

## Step 1: download / update pypi download stats

The package download counts are needed for heuristics in the guess algorithm during the generate step, also for upm to sort search results with. The file
`download_stats.json` file contains these stats and are checked in to git.
The stats are downloaded from a public BigQuery table made available
by Pypi. To download the stats and update the `download_stats.json` file:

```bash
go run ./gen_pypi_map bq -gcp <gcp-project-name>
```

The gcp-project-name can be any replit gcp project, because the table we are accessing `bigquery-public-data.pypi.file_downloads` is public. More info here: <https://packaging.python.org/en/latest/guides/analyzing-pypi-package-downloads/>

## Step 2: test modules

Next we test the packages in pypi we want to be able to guess. The default test
is:

0. use pkgutil to see what modules exists before installing the package
1. install the package
2. use pkgutil to see what new modules were added compared to before

We used to run this test on all modules on pypi. Now we have the option to
run it only on a subset of modules. The default is to collect the top
10000 packages. You can change this by passing in a different number to the
optional `-threshold` flag.

For example, to test the top 50000 packages:

```bash
go run ./gen_pypi_map/ test -threshold 50000
```

... or, to just test a single package:

```bash
go run ./gen_pypi_map test-one -package replit-object-storage
```

NB: The command will only test a package if its version has not already been
tested. To force a rerun, please use `-force`.

The results of each "test" run are stored in the directory,
specified by `-cache` (default: `./cache/`).

## Step 3: generate `pkgs.json`

Running the following command iterates over all "test" metadata accumulated in
`./cache/` and upserts that into `pkgs.json`. Commit all changes to `pkgs.json`

```bash
go run ./gen_pypi_map updatepkgs
```

If you want to force a retest of the packages, you can use the `-force` flag.

## Step 4: generate sqlite db

Finally, we use the collected data to generate the lookup database. This is
done at upm-build time, since the resulting database should not be committed
into git.

You'll find that...

1. `nix/upm/default.nix` calls
2. `make internal/backends/python/pypi_map.sqlite` which calls
3. `go run ./gen_pypi_map/ gen`

You should only need to do this locally when testing.
