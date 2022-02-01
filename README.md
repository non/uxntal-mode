# tal-mode

Emacs major mode for the [uxntal](https://wiki.xxiivv.com/site/uxntal.html) assembly language.

## installing the mode

Currently you must install the mode manually.

Copy `tal-mode.el` to the desired location and then modify `init.el`:

```elisp
;; ensure the directory containing tal-mode.el is mentioned
;; in emacs' load-path variable.
(add-to-list 'load-path "~/.emacs.d/lisp/")

;; then load tal-mode
(require 'tal-mode)
```

## features

Currently-supported features:

 * Syntax highlighting
 * Invoking `uxnasm` via `M-x compile`
 * Interpreting hex numbers as decimal (`C-c d`)

Future features:

 * Display instruction docs, stack effect
 * Support for goto-definition (`M-.`)

## see also

https://github.com/xaderfos/uxntal-mode
https://github.com/rafapaezbas/uxntal-mode
