# hlinecount

A simple command line tool to count non-empty lines in textfiles.


## Installation

- clone the repo
- `cabal install`


## Usage

Current command line options:

    Help Options:
      -h, --help
        Show option summary.
      --help-all
        Show all help options.

    Application Options:
      -r, --recursive :: bool
        Scan directories recursively.
        default: true
      -i, --ignore :: list<text>
        Ignore these paths.
        default: ".git/","build/","Setup.hs"
      -f, --files :: list<text>
        Fileextensions to include in the search.
        default: ".hs",".lhs"
      --ignore-hidden :: bool
        Ignore hidden files.
        default: true
