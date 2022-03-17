# wal-major-mode
An Emacs major mode for WAL

## Installation
To install clone this repository and add the directory to your emacs `load-path` in your emacs init file.

    (add-to-list 'load-path "~/path/to/wal-major-mode/")
    (require 'wal)

## Usage
Once installed the WAL major mode should automatically start once you open a ".wal" file, if not it can be started with .
To start the WAL interpreter execute `run-wal`.
The current buffer can be evaluated using `wal-eval-buffer`.

## Keybindings
    M-<tab>       auto completion
    C-x C-e       evaluate last expression
    C-M-x         evaluate next expression
