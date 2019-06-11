# UPM

Universal Package Manager. More info to come.

## Command-line interface

    $ upm [-h, -?, -help, --help, help]

Get help. When run with a subcommand, get help for that command.

    $ upm [-v, -V, -version, --version, version]

Get the version of UPM.

    $ upm [-l, --lang=LANG] CMD

Run a UPM command. Specify a language or have it auto-detected.
Multiple languages can be provided, in which case the selected command
is run for all of them.

    $ upm which-language

Ask what language UPM thinks your project is.

    $ upm list-languages

List the names of the languages that are supported by UPM.

    $ upm search QUERY...

Searches the package index. Print out a truncated list of package
names.

    $ upm (info | show) PACKAGE [-f, --format=(table|json)]

Show metadata about a package. Default to table format.

    $ upm add "PACKAGE[ SPEC]"... [-g, --guess]

Add packages to the specfile. Then update the lockfile and specfile,
and reinstall packages. You can give a specific version spec as part
of the same argument as the package name, delimited by a space, or
omit to use the default (the latest version, or something similarly
reasonable). If `--guess` is given, then `upm guess` is implicitly run
and the output is combined with the packages given on the command
line.

    $ upm remove PACKAGE...

Remove packages from the specfile. Then update the lockfile and the
specfile and reinstall packages.

    $ upm lock [-f, --force]

Update the lockfile from the specfile, if necessary. Then reinstall
packages from the lockfile. If `--force` is given, always update the
lockfile, even if the specfile has not been changed since the last
update.

    $ upm install [-f, --force]

Remove all existing installed packages, and install packages afresh
from the lockfile (if necessary). If `--force` is given, always
reinstall packages, even if the lockfile has not changed since the
last install.

    $ upm update

Alias for `upm lock --force`.

    $ upm list [-a, --all]

List packages from the specfile, with versions (if applicable). If
`--all` is given, list packages from the lockfile instead.

    $ upm guess [-a, --all]

Guess what packages are required by the project but not currently
listed in the specfile. Print each one on a separate line to stdout.
If `--all` is given, list also packages already in the specfile.

## Example usage

    $ upm which-language
    python-poetry

    $ upm list-languages
    python
    python-pip
    python-pipenv
    python-poetry
    nodejs
    nodejs-npm
    nodejs-yarn
    ruby-bundle

    $ upm search flask
    Flask
    Flask-Celery
    Flask-MakoTemplates
    flask-restly
    Flask-Mime

    $ upm info flask
    name          Flask
    description   A simple framework for building complex web applications.
    version       1.0.3
    homepage      https://www.palletsprojects.com/p/flask/
    docs          http://flask.pocoo.org/docs/
    source        https://github.com/pallets/flask
    bugs          https://github.com/pallets/flask/issues

    $ upm info flask --format=json
    {
      "name": "Flask",
      "description": "A simple framework for building complex web applications.",
      "version": "1.0.3",
      "homepage": "https://www.palletsprojects.com/p/flask/",
      "docs": "http://flask.pocoo.org/docs/",
      "source": "https://github.com/pallets/flask",
      "bugs": "https://github.com/pallets/flask/issues"
    }

    $ upm add flask
    Creating virtualenv upm-test-py3.7 in /Users/raxod502/Library/Caches/pypoetry/virtualenvs
    Using version ^1.0 for flask

    Updating dependencies
    Resolving dependencies... (0.1s)

    Writing lock file


    Package operations: 6 installs, 0 updates, 0 removals

      - Installing markupsafe (1.1.1)
      - Installing click (7.0)
      - Installing itsdangerous (1.1.0)
      - Installing jinja2 (2.10.1)
      - Installing werkzeug (0.15.4)
      - Installing flask (1.0.3)

    $ upm list
    Flask         1.0.3

    $ upm list --all
    Click         7.0
    Flask         1.0.3
    itsdangerous  1.1.0
    Jinja2        2.10.1
    MarkupSafe    1.1.1
    Werkzeug      0.15.4

## Feature matrix

|               | python-pip                | python-pipenv    | python-poetry  | nodejs-npm         | nodejs-yarn         | ruby-bundle               |
|---------------|:--------------------------|:-----------------|:---------------|:-------------------|:--------------------|:--------------------------|
| upm search    | pip search (?)            | pip search (?)   | pip search (?) | npm search         | npm search          | gem search                |
| upm info      | pypi info (?)             | pypi info (?)    | pypi info (?)  | npm view           | yarn info           | gem info -r               |
| upm add       | (1)                       | pipenv install   | poetry add     | npm install        | yarn add            | bundle add                |
| upm add -v    | (1)                       | pipenv install   | poetry add     | npm install        | yarn add            | bundle add                |
| upm remove    | (2)                       | pipenv uninstall | poetry remove  | npm uninstall      | yarn remove         | bundle remove             |
| upm lock      | (3)                       | pipenv update    | poetry update  | npm install        | yarn upgrade        | bundle update             |
| upm install   | (3)                       | pipenv sync      | poetry install | npm install        | yarn install        | bundle install            |
| upm list      | cat requirements.txt      |                  |                | npm list --depth=0 |                     |                           |
| upm list -a   | cat requirements-lock.txt | pip list         |                | npm list           | yarn list --depth=0 | bunder list               |

1. echo >> requirements.txt &&
   rm -rf VENV &&
   python -m venv VENV &&
   VENV/bin/pip install -r requirements.txt &&
   VENV/bin/pip freeze > requirements-lock.txt

2. sed -i requirements.txt &&
   rm -rf VENV &&
   python -m venv VENV &&
   VENV/bin/pip install -r requirements.txt &&
   VENV/bin/pip freeze > requirements-lock.txt

3. rm -rf VENV &&
   python -m venv VENV &&
   VENV/bin/pip install -r requirements.txt &&
   VENV/bin/pip freeze > requirements-lock.txt
