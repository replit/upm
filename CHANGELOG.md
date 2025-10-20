# Changelog

All notable changes to this project will be documented in this file.
The format is based on [Keep a
Changelog](https://keepachangelog.com/en/1.0.0/).

## 1.0 (released 2019-08-14)
### Features
* Languages and package managers:
  * Python 3 and Python 2 with Poetry (`python-python3-poetry`,
    `python-python2-poetry`; all features)
  * Node.js with NPM or Yarn (`nodejs-npm`, `nodejs-yarn`; all
    features)
  * Ruby with Bundler (`ruby-bundler`; all features except `guess`)
  * Emacs Lisp with Cask (`elisp-cask`; all features)
* Subcommands:
  * `which-language`
  * `list-languages`
  * `search`
  * `info`
  * `add`
  * `remove`
  * `lock`
  * `install`
  * `list`
  * `guess`
  * `show-specfile`
  * `show-lockfile`
  * `show-package-dir`
  * `help`
* Command-line options:
  * Add guessed packages
  * Bypass cache
  * Ignore specific packages
  * Override or filter language backends
  * Select between tabular and JSON output
  * Set verbosity
  * Upgrade instead of locking
