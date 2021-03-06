<a href="https://www.gnu.org/software/emacs/"><img src="https://www.gnu.org/software/emacs/images/emacs.png" alt="Emacs Logo" width="80" height="80" align="right"></a>
[![GitHub license](https://img.shields.io/github/license/damon-kwok/verona-mode?logo=gnu&.svg)](https://github.com/damon-kwok/verona-mode/blob/master/COPYING)
[![Sponsor](https://img.shields.io/badge/Support%20Me-%F0%9F%92%97-ff69b4.svg)](https://www.patreon.com/DamonKwok)
[![MELPA](http://melpa.org/packages/verona-mode-badge.svg)](http://melpa.org/#/verona-mode)
<!-- [![MELPA Stable](http://stable.melpa.org/packages/verona-mode-badge.svg)](http://stable.melpa.org/#/verona-mode) -->

# Verona Mode

An Emacs major mode for the [Verona](https://github.com/microsoft/verona/) programming language.

- Screenshot

![screenshot](https://github.com/damon-kwok/verona-mode/blob/master/screenshot.png)

## Features

- [x] Syntax highlighting (font-lock)
- [ ] Indentation
- [x] Workspace support
- [ ] Auto format on save
- [x] Compilation integration
- [x] Code navigation (using `imenu`)
- [x] Go to definition (using `ctags`)
- [x] Code completion (using `company-mode`)
<!-- - [x] Code folding -->
<!-- - [x] TODO highlighting -->
<!-- - [x] Rainbow delimiters -->
<!-- - [x] Whitespace character dsiplay -->
<!-- - [x] Fill column indicator -->
<!-- - [x] `V` mode menu -->

## Installation

### Using MELPA
This package can be obtain from
[MELPA](http://melpa.org/#/verona-mode) or
[MELPA Stable](http://stable.melpa.org/#/verona-mode). The `master`
branch is continuously deployed to `MELPA`, and released versions are
deployed to `MELPA Stable`.

<kbd>M-x package-install [RET] verona-mode [RET]</kbd>

Right now `verona-mode` doesn't take a lot of configuration (i.e.
it's too simple to need any).

```elisp
(require 'verona-mode)
(define-key verona-mode-map (kbd "<f6>")  verona-menu)
```

### Using [use-package](https://github.com/jwiegley/use-package) and [straight.el](https://github.com/raxod502/straight.el)

```elisp
(use-package verona-mode
  :straight (verona-mode
             :type git
             :host github
             :repo "damon-kwok/verona-mode"
             :files ("tokens" "verona-mode.el"))
  :config
  :bind-keymap
  ("<f6>" . verona-menu)
  :mode ("\\.verona\\'" . verona-mode))
```
