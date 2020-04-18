;;; auto-latex-snippets.el --- automatic expansion of latex macros -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020 Yoav Marco
;;
;; Author: Yoav Marco <http://github/yoavm448>
;; Maintainer: Yoav Marco <yoavm448@gmail.com>
;; Created: April 17, 2020
;; Modified: April 17, 2020
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/yoavm448/auto-latex-snippets
;; Package-Requires: ((emacs 27.0.90) (cl-lib "0.5"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  automatic expansion of latex macros
;;
;;; Code:

(require 'texmathp)

(defun als-expand-snippet (key expansion &optional condition)
  "Expand snippet with KEY as EXPANSION.

When CONDITION is a function, call it and do not expand if
returned nil."
  (when (or (null condition)
            (let (returned)
              (backward-char (length key))
              (setq returned (funcall condition))
              (forward-char (length key))
              returned))
    (delete-char (- (length key)))
    (if (functionp expansion)
        (funcall expansion)
      (insert expansion))))


(defun als-make-prefix-map (keymap key expansion condition)
  "Bind KEY as extended prefix in KEYMAP to EXPANTION.

EXPANTION must either be a string or function."
  (unless (or (stringp expansion) (functionp expansion))
    (error "Expansion must be either a string or function"))
  (unless (or (null condition) (functionp condition))
    (error "Condition must be either a string or function"))

  (let ((incomplete-key (substring key 0 -1))
        (orig-keymap keymap))
    (mapc (lambda (c)
            (setq keymap (or (lookup-key keymap (string c))
                             (define-key keymap (string c) (make-sparse-keymap)))))
          incomplete-key)
    (define-key keymap (substring key -1)
      (lambda () (als-expand-snippet key expansion condition)))
    orig-keymap))

(defun als-set-expanding-ligatures (keymap condition key-expantions)
  "Set multiple expantions for `als-set-expanding-ligature'.

KEYMAP is passed to `als-set-expanding-ligature', and
CONDITION is the condition applied to all of the KEY-EXPANTIONS,
which should be an alist of (key . expantion)."
  (dolist (key-expansion key-expantions)
    (als-make-prefix-map
     keymap (car key-expansion) (cdr key-expansion)
     condition)))

(defun als-insert-subscript ()
  "Expansion function used for auto-subscript snippets."
  (insert "_" (this-command-keys)))

(defun als-auto-index-condition ()
  "Condition usef for auto-subscript snippets."
  (and
   ;; Before is some indexable char
   (or (<= ?a (char-before) ?z)
       (<= ?A (char-before) ?Z))
   ;; Not a macro
   (not (save-excursion
          (and (search-backward "\\" (line-beginning-position) t)
               (looking-at "\\\\[a-zA-Z0-9*@]+")
               (<= (match-beginning 0) (point) (match-end 0)))))
   ;; Inside math
   (texmathp)))

(defvar als-prefix-map
  (let ((keymap (make-sparse-keymap)))
    (als-set-expanding-ligatures
     keymap #'texmathp
     '(
       ;; ("a1"       . "a_1")
       ;; ("a_11"     . "a_{11}")
       ;; ("a"        . "^ a^")
       ;; ("a^11"     . "a^{11}")
       ;; ("a+"       . "a +")
       ;; ("a"        . "+b a + b")
       ("..."      . "\\dots")
       ("=>"       . "`\\implies")
       ("=<"       . "\\impliedby")
       ;; ("//"       . "\\frac{}{}")
       ;; (".../"     . "\\frac{...}{}")
       ("iff"      . "\\iff")
       ("inn"      . "\\in")
       ("notin"    . "\\not\\in")
       ("!="       . "\\neq")
       ("=="       . "&=")
       ("~="       . "\\approx")
       ("~~"       . "\\sim")
       (">="       . "\\geq")
       ("<="       . "\\leq")
       (">>"       . "\\gg")
       ("<<"       . "\\ll")
       ("xx"       . "\\times")
       ("**"       . "\\cdot")
       ("->"       . "\\to")
       ("|->"      . "\\mapsto")
       ("!>"       . "\\mapsto")
       ;; ("v,."      . "\\vec{v}")
       ;; ("abar"     . "\\overline{a}")
       ;; ("ahat"     . "\\hat{a}")
       ;; ("a~"       . "\\tilde{a}")
       ;; ("a."       . "\\dot{a}")
       ;; ("a.."      . "\\ddot{a}")
       ;; ("...\\)a"  . "...\\) a")
       ("\\\\\\" . "\\setminus")
       ;; ("pmat"     . "pmatrix")
       ("part"     . "\\frac{\\partial }{\\partial }")
       ;; ("sq"       . "\\sqrt{}")
       ("sr"       . "^2")
       ("cb"       . "^3")
       ("EE"       . "\\exists")
       ("AA"       . "\\forall")
       ;; ("aii"      . "a_i")
       ;; ("aip1"     . "a_{i+1}")
       ;; ("set"      . "\\{ \\}")
       ("||"       . "\\mid")
       ("<>"       . "\\diamond")
       ;; ("case"     . "cases env.")
       ;; ("st"       . "\\text{s.t.}")
       ("+-"       . "\\pm")
       ("-+"       . "\\mp")
       ;; ("nCr"      . "\\binom{n}{r}")
       ))

    (als-set-expanding-ligatures
     keymap #'texmathp
     (mapcar (lambda (m) (cons m (concat "\\" m)))
             '("to" "sin" "cos" "arccot" "cot" "csc" "ln" "log" "exp" "star" "perp"
               "arcsin" "arccos" "arctan" "arccot" "arccsc" "arcsec" "int")))
    (als-set-expanding-ligatures
     keymap #'als-auto-index-condition
     '(("0" . als-insert-subscript)
       ("1" . als-insert-subscript)
       ("2" . als-insert-subscript)
       ("3" . als-insert-subscript)
       ("4" . als-insert-subscript)
       ("5" . als-insert-subscript)
       ("6" . als-insert-subscript)
       ("7" . als-insert-subscript)
       ("8" . als-insert-subscript)
       ("9" . als-insert-subscript)))
    keymap)
  "Defalut snippet keymap.")

(defvar als-current-prefix-map als-prefix-map
  "Global variable to keep track of the current user path trace of snippets.

Gets updated by `als-post-self-insert-hook'.")

(defun als-post-self-insert-hook ()
  "The `post-self-insert-hook' used to keey track of the user path to snippets.

The path is kept in `als-current-prefix-map'."
  (let ((k (lookup-key als-current-prefix-map (this-command-keys))))
    (setq als-current-prefix-map
          (cond ((null k)
                 als-prefix-map)
                ((keymapp k)
                 k)
                ((functionp k)
                 (funcall k)
                 als-prefix-map)))))

;;;###autoload
(define-minor-mode auto-latex-snippets-mode
  "Minor mode for dynamically auto-expanding LaTeX snippets.

See TODO for the availible snippets."
  :init-value nil
  (if auto-latex-snippets-mode
      (add-hook 'post-self-insert-hook #'als-post-self-insert-hook 0 t)
    (remove-hook 'post-self-insert-hook #'als-post-self-insert-hook t)))

(provide 'auto-latex-snippets)
;;; auto-latex-snippets.el ends here
