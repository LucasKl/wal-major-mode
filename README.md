# wal-major-mode
[![MELPA](https://melpa.org/packages/wal-mode-badge.svg)](https://melpa.org/#/wal-mode)

An Emacs major mode for [WAL](https://github.com/ics-jku/wal).

## Installation

### From MELPA
WAL mode can be installed directly from [MELPA](https://melpa.org/#/) after [adding the MELPA repository to your EMACS](https://melpa.org/#/getting-started).

The recommended way of installaing WAL mode is by using [use-package](https://github.com/jwiegley/use-package#installing-use-package) `(use-package wal-mode)`.

Alternatively, WAL mode can be installed manually using package-install using `M-x package-install RET wal-mode RET`.

### From Source
To install clone this repository and add the directory to your emacs `load-path` in your emacs init file.

    (add-to-list 'load-path "~/path/to/wal-major-mode/")
    (require 'wal-mode)

## Usage
Once installed the WAL major mode should automatically start once you open a ".wal" file, if not it can be started with `wal-mode`.
To start the WAL interpreter execute `run-wal`.
The current buffer can be evaluated using `wal-eval-buffer`.

## Keybindings
    M-<tab>       auto completion
    C-x C-e       evaluate last expression
    C-M-x         evaluate next expression
