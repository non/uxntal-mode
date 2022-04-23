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
 * Explaining Uxntal words (`tal-explain-word`)
   + Decodes instructions, showing their stack effects
   + Shows decimal values of numeric constants
   + Explains syntactic category (e.g. "sublabel definition")
 * Imenu support for macro definitions and labels

Future features:

 * Interactive evaluation
 * Support for goto-definition (`M-.`)
 * Decimal -> hexadecimal conversions
 * ASCII conversions
 * Input string literal as bytes

## settings

By default `tal-mode` is lax about comment highlighting. This means that some
invalid comments such as `(this)` or `(that )` or `( these)` will be highlighted
incorrectly.

If you would prefer to have stricter comment highlighting which forbids all
invalid comments (but may also forbid valid comments like `( )`) set
`tal-mode-strict-comments` to `t`.

Unfortunately both modes fail on words like `worst-case-(` due to limitations
in how Emacs handles multiline comments.

## screenshot

![tal-mode screenshot](http://plastic-idolatry.com/erik/nxu/tal-mode1.png)

## attribution

Copyright d_m, 2022.

This code is available to you under the
[Apache License Version 2.0](https://www.apache.org/licenses/LICENSE-2.0.txt).
See COPYING for more details.

## see also

 * https://github.com/xaderfos/uxntal-mode
 * https://github.com/rafapaezbas/uxntal-mode
