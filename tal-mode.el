;;; tal-mode.el --- Major mode for Uxntal assembly   -*- lexical-binding: t; -*-

;; Copyright (c) 2022 d_m

;; Author: d_m <d_m@plastic-idolatry.com>
;; Homepage: https://github.com/non/tal-mode
;; Version: 0.1
;; Package-Requires: ((emacs "27.1"))

;;; Commentary:

;; This major mode supports writing the Uxntal assmembly langauge as documented
;; here: https://wiki.xxiivv.com/site/uxntal.html

;; Prior art: https://github.com/xaderfos/uxntal-mode

;;; Code:

;; use rx for regular expressions
(require 'rx)
(require 'seq)

;; set up a mode hook
(defvar tal-mode-hook nil)

;; open .tal files with this mode
(add-to-list 'auto-mode-alist '("\\.tal\\'" . tal-mode))

;; macro definitions like %MOD
(defconst tal-mode-macro-define-re
  (rx (group bow "%" (1+ (not (in space))) eow)))

;; includes like ~util.tal
(defconst tal-mode-include-re
  (rx (group bow "~" (1+ (not (in space))) eow)))

;; labels like @foo
(defconst tal-mode-label-define-re
  (rx (group bow "@" (1+ (not (in space))) eow)))

;; subabels like &bar
(defconst tal-mode-sublabel-define-re
  (rx (group bow "&" (1+ (not (in space))) eow)))

;; raw characters like 'a or '[
(defconst tal-mode-raw-char-re
  (rx (group bow "'" (in "!-~") eow)))

;; raw strings like "foo or "a-b-c-d-e
(defconst tal-mode-raw-str-re
  (rx (group bow "\"" (1+ (in "!-~")) eow)))

;; absolute pads like |a0 or |0100
(defconst tal-mode-absolute-pad-re
  (rx (group
       bow "|"
       (repeat 2 (in "0-9a-f"))
       (\? (repeat 2 (in "0-9a-f")))
       eow)))

;; pads like $1 $1f $300 $1000
(defconst tal-mode-relative-pad-re
  (rx (group bow "$" (repeat 1 4 (in "0-9a-f")) eow)))

;; addresses such as .foo ,bar ;baz :qux
(defconst tal-mode-addr-zeropage-re
  (rx (group bow "." (1+ (not (in space))) eow)))
(defconst tal-mode-addr-relative-re
  (rx (group bow "," (1+ (not (in space))) eow)))
(defconst tal-mode-addr-absolute-re
  (rx (group bow ";" (1+ (not (in space))) eow)))
(defconst tal-mode-addr-raw-re
  (rx (group bow ":" (1+ (not (in space))) eow)))

;; literal numbers like #ff or #abcd
(defconst tal-mode-number-re
  (rx (group
       bow "#"
       (repeat 2 (in "0-9a-f"))
       (\? (repeat 2 (in "0-9a-f")))
       eow)))

;; raw numbers like ff or abcd
(defconst tal-mode-raw-number-re
  (rx (group
       bow
       (repeat 2 (in "0-9a-f"))
       (\? (repeat 2 (in "0-9a-f")))
       eow)))

;; tal instructions like ADD or JMP2r
(defconst tal-mode-inst-re
  (rx (group bow
       (or "BRK"
           (group "LIT" (\? "2") (\? "r"))
           (group (or "INC" "POP" "DUP" "NIP" "SWP" "OVR" "ROT"
                      "EQU" "NEQ" "GTH" "LTH"
                      "JMP" "JCN" "JSR" "STH"
                      "LDZ" "STZ" "LDR" "STR" "LDA" "STA"
                      "DEI" "DEO"
                      "ADD" "SUB" "MUL" "DIV"
                      "AND" "ORA" "EOR" "SFT")
                  (\? "2") (\? "k") (\? "r")))
       eow)))

;; all previous rules joined together into a list
(defconst tal-font-lock-keywords-1
  (list
   ;; macros (%)
   (list tal-mode-macro-define-re 1 font-lock-keyword-face)
   ;; addresses (. , ; :)
   (list tal-mode-addr-zeropage-re 1 font-lock-variable-name-face)
   (list tal-mode-addr-relative-re 1 font-lock-variable-name-face)
   (list tal-mode-addr-absolute-re 1 font-lock-variable-name-face)
   (list tal-mode-addr-raw-re 1 font-lock-variable-name-face)
   ;; labels (@ &)
   (list tal-mode-label-define-re 1 font-lock-function-name-face)
   (list tal-mode-sublabel-define-re 1 font-lock-function-name-face)
   ;; padding (| $)
   (list tal-mode-absolute-pad-re 1 font-lock-preprocessor-face)
   (list tal-mode-relative-pad-re 1 font-lock-preprocessor-face)
   ;; includes (~)
   (list tal-mode-include-re 1 font-lock-preprocessor-face)
   ;; instructions
   (list tal-mode-inst-re 1 font-lock-builtin-face)
   ;; constant numbers (#)
   (list tal-mode-number-re 1 font-lock-constant-face)
   ;; raw values (' ")
   (list tal-mode-raw-number-re 1 font-lock-string-face)
   (list tal-mode-raw-char-re 1 font-lock-string-face)
   (list tal-mode-raw-str-re 1 font-lock-string-face))
  "Level one font lock.")

;; create the syntax table which powers some highlighting decisions.
;;
;; since it is not possible to exactly encode uxntal's rules for
;; comments, we have to choose between strict and non-strict
;; highlighting.
;;
;; when strict, valid comments such as "( )" or "(<newline>)"
;; will be incorrectly rejected.
;;
;; when non-strict, invalid comments such as "(hi )" or "( bye)"
;; will be incorrectly accepted.
(defun tal-create-syntax-table (strict)
  "Create a syntax table. The STRICT parameter determines whether to use strict or lax parsing for comments."
  (let ((table (make-syntax-table))
        (c 0))
    ;; treat characters <= space as whitespace
    (while (< c ?!)
      (modify-syntax-entry c " " table)
      (setq c (1+ c)))
    ;; treat almost all printable characters as word characters
    (while (< c 127)
      (modify-syntax-entry c "w" table)
      (setq c (1+ c)))
    ;; when strict, we require '(' and ')' to have whitespace padding.
    ;; this is typically ok but fails on things like "( )" which
    ;; should be valid comments but would not highlight correctly.
    (if strict
      (progn
        (modify-syntax-entry ?\( "()1nb" table)
        (modify-syntax-entry ?\) ")(4nb" table)
        (modify-syntax-entry ?\s " 123" table)
        (modify-syntax-entry ?\t " 123" table)
        (modify-syntax-entry ?\n " 123" table))
      (progn
        (modify-syntax-entry ?\( "<)nb" table)
        (modify-syntax-entry ?\) ">(nb" table)))
    ;; generic delimiters, ignored by uxnasm
    (modify-syntax-entry ?\[ "(]" table)
    (modify-syntax-entry ?\] ")[" table)
    (modify-syntax-entry ?\{ "(}" table)
    (modify-syntax-entry ?\} "){" table)
    ;; return the syntax table
    table))

(defcustom tal-mode-strict-comments nil
  "When non-nil, will parse comments strictly, ensuring invalid comments are rejected. Otherwise, comments are parsed permissively, ensuring valid comments are accepted."
  :type 'boolean
  :safe #'booleanp
  :group 'tal)

(defvar tal-mode-syntax-table
  (tal-create-syntax-table tal-mode-strict-comments)
  "Syntax table in use in `tal-mode' buffers.")

;; set up mode
(defun tal-mode ()
  "Major mode for editing Tal files."
  (interactive)
  (kill-all-local-variables)
  (set-syntax-table tal-mode-syntax-table)
  (set (make-local-variable 'font-lock-defaults) '(tal-font-lock-keywords-1 nil nil))
  (setq major-mode 'tal-mode)
  (make-local-variable 'comment-start)
  (make-local-variable 'comment-end)
  (setq imenu-generic-expression
        (list (list nil tal-mode-label-define-re 1)
              (list nil tal-mode-macro-define-re 1)))
  (setq comment-start "( ")
  (setq comment-end " )")
  (setq mode-name "Tal")
  (run-hooks 'tal-mode-hook))

;; set up M-x compile to call uxnasm
(add-hook 'tal-mode-hook
  (lambda ()
    (let* ((in (file-relative-name buffer-file-name))
           (out (concat (file-name-sans-extension in) ".rom")))
        (set (make-local-variable 'compile-command)
             (concat "uxnasm " in " " out)))))

(add-hook 'tal-mode-hook
  (lambda ()
    (setq tal-mode-syntax-table (tal-create-syntax-table tal-mode-strict-comments))
    (set-syntax-table tal-mode-syntax-table)))

;; Constructs a table of metadata about every instruction.
;; This table powers tal-decode-instruction.
(defconst tal-mode-instructions
  (let ((m (make-hash-table :test 'equal :size 32)))
    (puthash "BRK" (vector "Break" '(() . ()) nil "halt the program") m)
    (puthash "LIT" (vector "Literal" '(() . ("a")) nil "push the next value onto the stack") m)
    (puthash "INC" (vector "Increment" '(("a") . ("b")) nil "adds one to the top of the stack") m)
    (puthash "POP" (vector "Pop" '(("a") . ()) nil "remove the top of the stack") m)
    (puthash "DUP" (vector "Duplicate" '(("a") . ("a" "a")) nil "duplicate the top of the stack") m)
    (puthash "NIP" (vector "Nip" '(("a" "b") . ("b")) nil "remove the second value (a)") m)
    (puthash "SWP" (vector "Swap" '(("a" "b") . ("b" "a")) nil "swap the top two stack values") m)
    (puthash "OVR" (vector "Over" '(("a" "b") . ("a" "b" "a")) nil "duplicate the second value (a) to the top of the stack") m)
    (puthash "ROT" (vector "Rotate" '(("a" "b" "c") . ("b" "c" "a")) nil "rotate the top three values to the left") m)
    (puthash "EQU" (vector "Equal" '(("a" "b") . ("bool^")) nil "push 01 if a == b; push 00 otherwise") m)
    (puthash "NEQ" (vector "Not Equal" '(("a" "b") . ("bool^")) nil "push 01 if a != b; push 00 otherwise") m)
    (puthash "LTH" (vector "Less Than" '(("a" "b") . ("bool^")) nil "push 01 if a < b; push 00 otherwise") m)
    (puthash "GTH" (vector "Greater Than" '(("a" "b") . ("bool^")) nil "push 01 if a > b; push 00 otherwise") m)
    (puthash "JMP" (vector "Jump" '(("addr") . ()) nil "modify the pc using addr") m)
    (puthash "JCN" (vector "Jump Conditional" '(("bool^" "addr") . ()) nil "if bool != 00, modify the pc using addr") m)
    (puthash "JSR" (vector "Jump Stash Return" '(("addr") . ()) '(() . ("pc")) "store pc onto return stack; modify pc using addr") m)
    (puthash "STH" (vector "Stash" '(("a") . ()) '(() . ("a")) "move the top of the stack to the return stack") m)
    (puthash "LDZ" (vector "Load Zero-Page" '(("addr^") . ("val")) nil "load value from first 256 bytes of memory onto the stack") m)
    (puthash "STZ" (vector "Store Zero-Page" '(("val" "addr^") . ()) nil "write top of stack into the first 256 bytes of memory") m)
    (puthash "LDR" (vector "Load Relative" '(("addr^") . ("val")) nil "load relative address onto the stack") m)
    (puthash "STR" (vector "Store Relative" '(("val" "addr^") . ()) nil "write top of stack to relative address") m)
    (puthash "LDA" (vector "Load Absolute" '(("addr*") . ("val")) nil "load absolute address onto the stack") m)
    (puthash "STA" (vector "Store Absolute" '(("val" "addr*") . ()) nil "write top of stack to absolute address") m)
    (puthash "DEI" (vector "Device In" '(("addr^") . ("val")) nil "load from the given device onto the stack") m)
    (puthash "DEO" (vector "Device Out" '(("val" "addr^") . ()) nil "write top of stack to the given device") m)
    (puthash "ADD" (vector "Add" '(("a" "b") . ("a+b")) nil "addition (a + b)") m)
    (puthash "SUB" (vector "Subtract" '(("a" "b") . ("a-b")) nil "subtraction (a - b)") m)
    (puthash "MUL" (vector "Multiply" '(("a" "b") . ("a*b")) nil "multiplication (a * b)") m)
    (puthash "DIV" (vector "Divide" '(("a" "b") . ("a/b")) nil "division (a / b)") m)
    (puthash "AND" (vector "And" '(("a" "b") . ("a&b")) nil "bitwise-and (a & b)") m)
    (puthash "ORA" (vector "Or" '(("a" "b") . ("a|b")) nil "bitwise-or (a | b)") m)
    (puthash "EOR" (vector "Exclusive Or" '(("a" "b") . ("a^b")) nil "bitwise-xor (a ^ b)") m)
    (puthash "SFT" (vector "Shift" '(("a" "b^") . ("c")) nil "bitshift right (b & 0xf) then left (b >> 4)") m)
    m))

(defun tal-format-stack (pair glyph)
  "Format the given stack PAIR as stack effects using GLYPH. Stacks are represented as a pair of lists for input and output parameters respectively. We apply GLYPH to any parameter that doesn't already have a suffix denoting its type ('^' for 8-bit values, '*' for 16-bit values)."
  (let* ((decorate (lambda (name) (if (or (string-suffix-p "^" name)
                                          (string-suffix-p "*" name))
                                      name
                                    (concat name glyph))))
         (ins (mapconcat decorate (car pair) " "))
         (outs (mapconcat decorate (cdr pair) " ")))
    (format "%s -> %s" ins outs)))

(defun tal-setup-keep (pair)
  "Translate the given stack PAIR for keep mode. This involves prepending the input parameters (i.e. the first list) to the output parameters (the second list)."
  (let ((in (car pair))
        (out (cdr pair)))
    (cons in (append in out))))

(defun tal-decode-instruction (inst)
  "Decode the meaning of the INST instruction. Instructions are always three capital letters followed by a suffix involving '2', 'k', and/or 'r'."
  (let ((m (string-match tal-mode-inst-re inst)))
    (if (eq m nil)
        (message "`%s' is not an instruction" inst)
      (let* ((base (substring inst 0 3))
             (inst-info (gethash base tal-mode-instructions))
             (name (aref inst-info 0))
             (s0 (aref inst-info 1))
             (s1 (aref inst-info 2))
             (doc (aref inst-info 3))
             ;; set up stacks based on the bitflags given
             ;; 2 -> use 16-bit values (*) instead of 8-bit (^)
             ;; k -> keep inputs on stack
             ;; r -> swap working stack (ws) and return stack (rs)
             (wsx (if (seq-contains-p inst ?r) s1 s0))
             (ws (if (seq-contains-p inst ?k) (tal-setup-keep wsx) wsx))
             (rsx (if (seq-contains-p inst ?r) s0 s1))
             (rs (if (seq-contains-p inst ?k) (tal-setup-keep rsx) rsx))
             (glyph (if (seq-contains-p inst ?2) "*" "^"))
             (wss (if ws (concat "(" (tal-format-stack ws glyph) ") ") ""))
             (rss (if rs (concat "{" (tal-format-stack rs glyph) "} ") "")))
        ;; create full string representation of stacks,
        ;; with delimiters and whitespace
        (message "%s %s%s%s: %s" inst wss rss name doc)))))

(defun tal-explain-word ()
  "Explain the given word's meaning in Uxntal. Depdending on the word, this may decode an instruction, display a numeric constant, or describe the syntactic category for the given word."
  (interactive)
  (let* ((w (current-word t t))
         (dec (lambda () (string-to-number w 16)))
         (dec1 (lambda () (string-to-number (substring w 1) 16))))
    (cond
     ((eq w nil) (message "No word selected"))
     ((string-match tal-mode-macro-define-re w) (message "%s is a macro definition" w))
     ((string-match tal-mode-include-re w) (message "%s is an include" w))
     ((string-match tal-mode-label-define-re w) (message "%s is a label definition" w))
     ((string-match tal-mode-sublabel-define-re w) (message "%s is a sublabel definition" w))
     ((string-match tal-mode-raw-char-re w) (message "%s is a raw char" w))
     ((string-match tal-mode-raw-str-re w) (message "%s is a raw string" w))
     ((string-match tal-mode-absolute-pad-re w) (message "%s is an absolute pad (%d)" w (funcall dec1)))
     ((string-match tal-mode-relative-pad-re w) (message "%s is a relative pad (+%d)" w (funcall dec1)))
     ((string-match tal-mode-addr-zeropage-re w) (message "%s is a zero-page address" w))
     ((string-match tal-mode-addr-relative-re w) (message "%s is a relative address" w))
     ((string-match tal-mode-addr-absolute-re w) (message "%s is an absolute address" w))
     ((string-match tal-mode-addr-raw-re w) (message "%s is a raw address" w))
     ((string-match tal-mode-number-re w) (message "%s is a number (%d)" w (funcall dec1)))
     ((string-match tal-mode-raw-number-re w) (message "%s is a raw number (%d)" w (funcall dec)))
     ((string-match tal-mode-inst-re w) (tal-decode-instruction w))
     (t (message "Unknown word: `%s'" w)))))

;; provide mode
(provide 'tal-mode)

;;; tal-mode.el ends here
