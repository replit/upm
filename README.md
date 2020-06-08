# UPM

![upm](https://amasad.me/public/images/long.png)
[![GoDoc](https://godoc.org/github.com/replit/upm?status.svg)](https://godoc.org/github.com/replit/upm)
[![Run on Repl.it](https://repl.it/badge/github/replit/upm)](https://repl.it/github/replit/upm)

UPM is the **Universal Package Manager**. It allows you to manage
packages for any (supported) programming language through the same
interface following the [principle of least
astonishment](https://en.wikipedia.org/wiki/Principle_of_least_astonishment).
At [Repl.it](https://repl.it/), we use UPM to provide deep package
manager integration for many different programming languages using the
same infrastructure.

UPM does not implement package management itself. Instead, it runs a
package manager for you. The value added is:

* you don't have to figure out whether to use Pip or Pipenv or Poetry
  to manage your Python packages or wade into the Cabal-versus-Stack
  holy war in Haskell-land
* you don't have to puzzle out why `pip search flask` doesn't return
  Flask in the search results
* you don't have to debug Bundler silently dropping your command-line
  options if you don't specify them in the right (undocumented) order
* you don't have to think about why the developers of NPM and Yarn
  decided to implement two completely different and mutually
  incompatible behaviors for `list --depth=0`, neither of which is
  exactly what you want
* you don't have to investigate what format the Yarn lockfile is in
  (turns out: almost YAML, but not quite)
* et cetera (I could go on all day)

In other words, UPM eliminates the need to remember a huge collection
of language-specific package manager quirks and weirdness, and adds a
few nifty extra features like dependency guessing and
machine-parseable specfile and lockfile listing.

## Supported languages

* Core: `upm add`, `upm remove`, `upm lock`, `upm install`, `upm list`
* Index: `upm search`, `upm info`
* Guess: `upm guess`

|                       | core | index | guess |
|-----------------------|------|-------|-------|
| python-python3-poetry | yes  | yes   | yes   |
| python-python2-poetry | yes  | yes   | yes   |
| nodejs-yarn           | yes  | yes   | yes   |
| nodejs-npm            | yes  | yes   | yes   |
| ruby-bundler          | yes  | yes   |       |
| elisp-cask            | yes  | yes   | yes   |
| dart-pub.dev          | yes  | yes   |       |
| rlang                 | yes  | yes   |       |

## Installation

You have many options. UPM is a single binary with no dependencies, so
you can install it anywhere. Tarballs are available on the [releases
page](https://github.com/replit/upm/releases). Read on for
instructions on installing via a package manager.

### macOS

Available on [Homebrew](https://brew.sh/) in a [custom
tap](https://docs.brew.sh/Taps).

    $ brew install replit/tap/upm

### Debian-based Linux

.deb packages are available on the [releases
page](https://github.com/replit/upm/releases).

### RPM-based Linux

.rpm packages are available on the [releases
page](https://github.com/replit/upm/releases).

### Arch Linux

Soon to be available on the [Arch User
Repository](https://aur.archlinux.org/). Right now, you can clone this
repository and install with `makepkg` using the PKGBUILD in
[`packaging/aur`](packaging/aur).

### Windows

Available on [Scoop](https://scoop.sh/) in a [custom
bucket](https://github.com/lukesampson/scoop/wiki/Buckets).

    $ scoop bucket add replit https://github.com/replit/scoop-bucket.git
    $ scoop install upm

### Snappy

Soon to be available on the [Snap Store](https://snapcraft.io/store).
Right now, .snap packages are available on the [releases
page](https://github.com/replit/upm/releases).

### Docker

You can try out UPM right away in a Docker image based on Ubuntu that
has all the supported languages and package managers already
installed.

    $ docker run -it --rm replco/upm

Additional tags are also available. `replco/upm:full` is the same as
the above, while `replco/upm:light` just has the UPM binary installed
to `/usr/local/bin` and none of the languages or package managers
installed. If you want to run a specific tagged release, rather than
the latest development snapshot, use e.g. `replco/upm:1.0`,
`replco/upm:1.0-full`, or `replco/upm:1.0-light`.

## Quick start

Let's create a new Python project:

    $ mkdir ~/python
    $ cd ~/python

We'll start by adding Flask as a dependency. UPM will handle setting
up the project for us:

    $ upm -l python add flask
    --> python3 -m poetry init --no-interaction

    This command will guide you through creating your pyproject.toml config.


    --> python3 -m poetry add flask
    Creating virtualenv python-py3.7 in /root/.cache/pypoetry/virtualenvs
    Using version ^1.1 for flask

    Updating dependencies
    Resolving dependencies... (0.6s)

    Writing lock file


    Package operations: 6 installs, 0 updates, 0 removals

      - Installing markupsafe (1.1.1)
      - Installing click (7.0)
      - Installing itsdangerous (1.1.0)
      - Installing jinja2 (2.10.1)
      - Installing werkzeug (0.15.4)
      - Installing flask (1.1.1)

UPM operates on a *specfile* and *lockfile* for each project. The
specfile says what your project's dependencies are in a human-readable
format, while the lockfile specifies exact versions for everything,
including transitive dependencies. For Python, the specfile is
`pyproject.toml` and the lockfile is `poetry.lock`:

    $ ls
    poetry.lock  pyproject.toml

We don't have to read them ourselves, because UPM can handle that.
Notice that UPM is now aware that our project uses Python, because of
the files that were created:

    $ upm list
    name    spec
    -----   ----
    flask   ^1.1

    $ upm list -a
    name           version
    ------------   -------
    click          7.0
    flask          1.1.1
    itsdangerous   1.1.0
    jinja2         2.10.1
    markupsafe     1.1.1
    werkzeug       0.15.4

Let's search for another dependency to add:

    $ upm search nose
    --> python3 -c '<secret sauce>' nose
    Name                Description                                                              Version
    -----------------   ----------------------------------------------------------------------   -------
    nose                nose extends unittest to make testing easier                             1.3.7
    nose-detecthttp     A nose plugin to detect tests making http calls.                         1.1.0
    nose-picker         nose plugin that picks a subset of your unit tests                       0.5.5
    nose-progressive    A testrunner with a progress bar and smarter tracebacks                  1.5.2
    nose-unittest       UNKNOWN                                                                  0.1.1
    nose-blockage       Raise errors when communicating outside of tests                         0.1.2
    nose-watcher        A nose plugin to watch for changes within the local directory.           0.1.3
    nose-bisect         A Nose plugin which allows bisection of test failures.                   0.1.0
    nose-printlog       Print log to console in nose tests                                       0.2.0
    nose-json           A JSON report plugin for Nose.                                           0.2.4
    nose-faulthandler   Nose plugin. Activates faulthandler module for test runs.                0.1
    nose-knows                                                                                   0.2
    nose-pagerduty      PagerDuty alert plugin for nose                                          0.2.0
    nose-logpertest     Logging nose plugin to create log per test                               0.0.1
    nose-bleed          A progressive coverage plugin for Nose.                                  0.5.1
    nose-numpyseterr    Nose plugin to set how floating-point errors are handled by numpy        0.1
    nose-skipreq        nose plugin that will skip Google API RequestError exceptions.           2.0
    nose-selecttests    Specify whitelist of keywords for tests to be run by nose                0.5
    nose-pacman         A testrunner with a pacman progress bar                                  0.1.0
    nose-switch         Add special switches in code, based on options set when running tests.   0.1.5

We can get more information about a package like this:

    $ upm info nose
    --> python3 -c '<secret sauce>' nose
    Name:          nose
    Description:   nose extends unittest to make testing easier
    Version:       1.3.7
    Homepage:      http://readthedocs.org/docs/nose/
    Author:        Jason Pellerin <jpellerin+nose@gmail.com>
    License:       GNU LGPL

For piping into other programs, the `search` and `info` commands can
also output JSON:

    $ upm info nose --format=json | jq
    --> python3 -c '<secret sauce>' nose
    {
      "name": "nose",
      "description": "nose extends unittest to make testing easier",
      "version": "1.3.7",
      "homepageURL": "http://readthedocs.org/docs/nose/",
      "author": "Jason Pellerin <jpellerin+nose@gmail.com>",
      "license": "GNU LGPL"
    }

UPM can also look at your project's source code and guess what
packages need to be installed. We use this on Repl.it to help
developers get started faster. To see it in action, we'll need some
source code:

    $ git clone https://github.com/replit/play.git ~/play
    $ cd ~/play
    $ upm add --guess
    --> python3 -c '<secret sauce>' '<secret sauce>'
    --> python3 -m poetry init --no-interaction

    This command will guide you through creating your pyproject.toml config.


    --> python3 -m poetry add pygame pymunk setuptools
    Creating virtualenv play-py3.7 in /root/.cache/pypoetry/virtualenvs
    Using version ^1.9 for pygame
    Using version ^5.5 for pymunk
    Using version ^41.0 for setuptools

    Updating dependencies
    Resolving dependencies... (1.4s)

    Writing lock file


    Package operations: 4 installs, 0 updates, 0 removals

      - Installing pycparser (2.19)
      - Installing cffi (1.12.3)
      - Installing pygame (1.9.6)
      - Installing pymunk (5.5.0)

You can also just get the list of guessed dependencies, if you want.
The `-a` flag lists all guessed dependencies, even the ones already
added to the specfile:

    $ upm guess -a
    pygame
    pymunk
    setuptools

All of this might seem a bit too simple to justify a new tool, but the
real power of UPM is that it works exactly the same for every
programming language:

    $ upm -l nodejs info express
    Name:          express
    Description:   Fast, unopinionated, minimalist web framework
    Version:       4.17.1
    Homepage:      http://expressjs.com/
    Source code:   git+https://github.com/expressjs/express.git
    Bug tracker:   https://github.com/expressjs/express/issues
    Author:        TJ Holowaychuk <tj@vision-media.ca>
    License:       MIT

    $ upm -l ruby info jekyll
    --> ruby -e '<secret sauce>' jekyll
    Name:            jekyll
    Description:     Jekyll is a simple, blog aware, static site generator.
    Version:         3.8.6
    Homepage:        https://github.com/jekyll/jekyll
    Documentation:   http://jekyllrb.com
    Source code:     https://github.com/jekyll/jekyll
    Bug tracker:     https://github.com/jekyll/jekyll/issues
    Author:          Tom Preston-Werner
    License:         MIT
    Dependencies:    addressable, colorator, em-websocket, i18n, jekyll-sass-converter, jekyll-watch, kramdown, liquid, mercenary, pathutil, rouge, safe_yaml

    $ upm -l elisp info elnode
    --> emacs -Q --batch --eval '<secret sauce>' /tmp/elpa552971126 info elnode
    Name:           elnode
    Description:    The Emacs webserver.
    Version:        20190702.1509
    Dependencies:   web, dash, noflet, s, creole, fakir, db, kv

That includes adding and removing packages, listing the specfile and
lockfile, searching package indices, and guessing project
dependencies. UPM knows all the best practices for each language so
that you don't have to!

## Usage

Explore the command-line interface at your leisure:

    $ upm --help
    Usage:
      upm [command]

    Available Commands:
      which-language   Query language autodetection
      list-languages   List supported languages
      search           Search for packages online
      info             Show package information from online registry
      add              Add packages to the specfile
      remove           Remove packages from the specfile
      lock             Generate the lockfile from the specfile
      install          Install packages from the lockfile
      list             List packages from the specfile (or lockfile)
      guess            Guess what packages are needed by your project
      show-specfile    Print the filename of the specfile
      show-lockfile    Print the filename of the lockfile
      show-package-dir Print the directory where packages are installed
      help             Help about any command

    Flags:
      -h, --help                       display command-line usage
          --ignored-packages strings   packages to ignore when guessing (comma-separated)
      -l, --lang string                specify project language(s) manually
      -q, --quiet                      don't show what commands are being run
      -v, --version                    display command version

    Use "upm [command] --help" for more information about a command.

Here are useful things to know that aren't obvious:

* **Language detection:** Your project's language is autodetected by
  the files in the current directory. This can be overridden either
  partially or completely by specifying a value for the `-l` option.
  You can see the available languages by running `upm list-languages`.
  In addition to a full language (e.g. `python-python3-poetry`), you
  can specify something simpler (e.g. `python`, `python3`, `python2`,
  `poetry`, `python-poetry`). In that case, UPM will examine all of
  the matching languages and pick whichever one it thinks is best. You
  can experiment with this logic by providing the `-l` option to `upm
  which-language`.
* **Information flow:** Conceptually, information about packages flows
  one way in UPM: add/remove -> specfile -> lockfile -> installed
  packages. You run `upm add` and `upm remove`, which modifies the
  specfile, and then the lockfile is automatically generated from the
  specfile, and then packages are automatically installed or
  uninstalled according to the lockfile. Just running `upm add` or
  `upm remove` will automatically perform all of these steps. Skipping
  steps is unfortunately not supported, because few package managers
  support that. You can however run only later steps in the pipeline
  by means of the `upm lock` and `upm install` commands.
* **Caching:** UPM maintains a simple JSON cache in the `.upm`
  subdirectory of your project, in order to improve performance. This
  is used to (1) skip generating the lockfile from the specfile if the
  specfile hasn't changed since last time; (2) skip reinstalling
  packages from the lockfile if the lockfile hasn't changed since last
  time; and (3) skip doing a full analysis of your code on `upm guess`
  if your imports haven't actually changed since last time (according
  to a quick regexp search). To reset the cache, you can delete that
  directory. However, this shouldn't be necessary very often, because
  you can use the `--force-lock` and `--force-install` options to `upm
  add`, `upm remove`, `upm lock`, and `upm install` (it is just
  `--force` for `upm install` due to lack of ambiguity) in order to
  ignore the cache for cases (1) and (2).

### Environment variables respected

* `UPM_PROJECT`: path to top-level directory containing project files.
  UPM uses this as its working directory. Defaults to the first parent
  directory containing a directory entry named `.upm` (like Git
  searches for `.git`), or the current directory if `.upm` is not
  found.
* `UPM_PYTHON2`: if nonempty, use instead of `python2` when invoking
  Python 2.
* `UPM_PYTHON3`: if nonempty, use instead of `python3` when invoking
  Python 3.
* `UPM_SILENCE_SUBROUTINES`: if nonempty, then enable `-q` when
  running commands that are not directly related to the operation the
  user requested (e.g. if running `upm add`, enable `-q` when reading
  the specfile to check which packages are already added).
* `UPM_STORE`: path of file used to store the JSON cache file,
  relative or absolute. Defaults to `.upm/store.json`.

## Dependencies

UPM itself has no dependencies. It is a single statically-linked
binary. However, if you wish to actually use it to manage packages for
a language, then the relevant language package manager needs to be
installed, as follows:

* `python-python3-poetry`/`python-python2-poetry`
  * [Python 2/3](https://www.python.org/)
  * [Pip](https://pip.pypa.io/en/stable/) for appropriate version(s)
    of Python
  * [Poetry](https://poetry.eustace.io/) for appropriate version(s) of
    Python
* `nodejs-yarn`
  * [Node.js](https://nodejs.org/en/)
  * [Yarn](https://yarnpkg.com/en/) for Yarn backend
  * [NPM](https://www.npmjs.com/get-npm) for NPM backend
* `ruby-bundler`
  * [Ruby](https://www.ruby-lang.org/en/)
  * [Bundler](https://bundler.io/)
  * [json](https://ruby-doc.org/stdlib/libdoc/json/rdoc/JSON.html)
* `elisp-cask`
  * [Emacs](https://www.gnu.org/software/emacs/)
  * [Cask](https://github.com/cask/cask)
  * [curl](https://curl.haxx.se/) (for `search` and `info`)
  * [SQLite](https://www.sqlite.org/index.html) (for `guess`)

All of these dependencies are already installed in the
`replco/upm:full` Docker image.

## Contributing

    $ make help
    usage:
      make upm       Build the UPM binary
      make dev       Run a shell with UPM source code and all package managers inside Docker
      make light     Build a Docker image with just the UPM binary
      make full      Build a Docker image with the UPM binary and all package managers
      make doc       Open Godoc in web browser
      make deploy    Publish UPM snapshot Docker images to Docker Hub
      make pkgbuild  Update and test PKGBUILD
      make clean     Remove build artifacts
      make help      Show this message

To build UPM, run `make upm` (or just `make`). This requires an
installation of [Go](https://golang.org/). Then add the directory
`./cmd/upm` to your `$PATH` so that you can run the binary. To remove
build artifacts, run `make clean`.

You can use [Docker](https://www.docker.com/) to avoid needing to
install the package managers that UPM drives. To do this, run `make
dev`. This will build an image and launch a shell inside the container
with the UPM source directory on your computer synced with the
filesystem inside the container. The same Makefile targets are
available, and UPM is added to the `$PATH` automatically. You only
need to restart the shell if you edit the Dockerfile or the scripts
used by the Dockerfile. Aliases available inside the shell:

* `l`: `ls -lAhF`
* `mt`: create temporary directory and cd to it (convenient for
  switching to a new "project" context)
* `u`: build UPM binary if source code has been modified, then run
  with given arguments
* `ub`: same as `u`, but force rebuilding binary (may be useful if you
  previously built outside the Docker container)

To build a Docker image which has only the UPM binary, for embedding
in other images, run `make light`. The image will be tagged as
`upm:light`. Alternatively, to build a Docker image which has the
binary and all the package managers, but not the UPM source code, run
`make full`. The image will be tagged as `upm:full`. These two images
are automatically built and deployed to [Docker
Hub](https://hub.docker.com/r/replco/upm) when a commit is merged to
`master`.

UPM does not currently have any tests; however, we plan to fix this.

### Deployment

Whenever a commit is merged to `master`, snapshot Docker images are
built and pushed to Docker Hub by CircleCI. Whenever a tagged release
(e.g. `v1.0`) is pushed to GitHub, the following happens:

* Release Docker images are tagged and pushed to Docker Hub.
* The [changelog](CHANGELOG.md) is parsed and published as a GitHub
  Release with the following assets:
  * source code (.zip and .tar.gz)
  * binary (.tar.gz; darwin, freebsd, linux, and windows; 386 and
    amd64)
  * Debian package (.deb; 386 and amd64)
  * RPM package (.rpm; 386 and amd64)
  * Snappy package (.snap; 386 and amd64)
  * checksums (.txt)
* The [Repl.it Homebrew tap](https://github.com/replit/homebrew-tap)
  is updated.
* The [Repl.it Scoop bucket](https://github.com/replit/scoop-bucket)
  is updated.

Once you push a release and it passes CI, the following must be done
**manually**:

* Edit [`packaging/aur/PKGBUILD`](packaging/aur/PKGBUILD) with the new
  version and run `make pkgbuild` to update and test the AUR package.
  Then push a new commit. (Once we are allowed to publish to AUR, you
  should also push `packaging/aur` there.)

<!--  Local Variables:   -->
<!--  truncate-lines: t  -->
<!--  End:               -->
