# UPM

Universal Package Manager. More info to come.

## Build and run

    % make help
    usage:
      make upm    Build the UPM binary
      make dev    Run a shell with UPM source code and all package managers inside Docker
      make light  Build a Docker image with just the UPM binary
      make full   Build a Docker image with the UPM binary and all package managers
      make clean  Remove build artifacts
      make help   Show this message

To build UPM, run `make` (or `make upm`). Then add the directory
`./cmd/upm` to your `$PATH` so that you can run the binary. To remove
build artifacts, run `make clean`.

You can use [Docker](https://www.docker.com/) to avoid needing to
install the package managers that UPM drives. To do this, run `make
dev`. This will build an image and launch a shell inside the container
with the UPM source directory on your computer synced with the
filesystem inside the container. The same Makefile targets are
available, and UPM is added to the `$PATH` automatically. You only
need to restart the shell if you edit the Dockerfile or the scripts
used by the Dockerfile.

To build a Docker image which has only the UPM binary, for embedding
in other images, run `make light`. The image will be tagged as
`upm:light`. Alternatively, to build a Docker image which has the
binary and all the package managers, but not the UPM source code, run
`make full`. The image will be tagged as `upm:full`.

## Dependencies

* `python-python3-poetry`/`python-python2-poetry`
  * [Python 2/3](https://www.python.org/)
  * [Pip](https://pip.pypa.io/en/stable/) for appropriate version(s)
    of Python
  * [Poetry](https://poetry.eustace.io/) for appropriate version(s) of
    Python
  * [Pipreqs](https://github.com/bndr/pipreqs) for appropriate
    version(s) of Python (for `guess`)
* `nodejs-yarn`
  * [Node.js](https://nodejs.org/en/)
  * [Yarn](https://yarnpkg.com/en/)
* `ruby-bundler`
  * [Ruby](https://www.ruby-lang.org/en/)
  * [Bundler](https://bundler.io/)
  * [json](https://ruby-doc.org/stdlib/libdoc/json/rdoc/JSON.html)
  * [gems](https://rubygems.org/gems/gems) (for `search` and `info`)
* `elisp-cask`
  * [Emacs](https://www.gnu.org/software/emacs/)
  * [Cask](https://github.com/cask/cask)
  * [SQLite](https://www.sqlite.org/index.html) (for `guess`)

## Feature matrix

* Core: `upm add`, `upm remove`, `upm lock`, `upm install`, `upm list`
* Index: `upm search`, `upm info`
* Guess: `upm guess`
* Global: `--global`

|                       | core | index | guess | global |
|-----------------------|------|-------|-------|--------|
| python-python3-poetry | yes  | yes   | yes   | yes    |
| python-python2-poetry | yes  | yes   | yes   | yes    |
| nodejs-yarn           | yes  | yes   | yes   |        |
| ruby-bundler          | yes  | yes   |       |        |
| elisp-cask            | yes  | yes   | yes   |        |

## Backend concepts

Each backend implements three important operations: `add/remove`,
`lock`, and `install`. Ideally, `add/remove` would only modify the
specfile, `lock` would only update the lockfile from the specfile, and
`install` would only install packages from the lockfile.
Unfortunately, existing package management infrastructure is
insufficiently expressive and powerful to realize this ideal, so UPM
has to deal with a variety of different ways backends may implement
this functionality; for example:

* `add/remove` may also update the lockfile and specfile.
* `lock` may also install packages.
* `install` may also update the lockfile (yes, really).

UPM deals with this problem by having each backend give some hints
about its behavior. This is done through a `quirks` operation which
returns a bitmask. Supported bits are:

* `quirksNotReproducible`: it's not possible to install from a
  lockfile; it's only possible to install from the specfile and then
  update the lockfile from what was installed.

## Deploy

The `upm:light` and `upm:full` images are automatically deployed to
[Docker Hub](https://hub.docker.com/r/replco/upm) when a commit is
merged to `master`.

<!--  Local Variables:   -->
<!--  truncate-lines: t  -->
<!--  End:               -->
