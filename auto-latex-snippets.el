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
;; Homepage: https://github.com/tecosaur/auto-latex-snippets
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
returned nil.

EXPANTION is called interactively, and CONDITION non-interactively."
  (when (and
         ;; key was fully typed
         (save-excursion
           (search-backward key (- (point) (length key)) t))
         ;; condition is either not present, or evaluates to true
         (or (null condition)
             (let (returned)
               (backward-char (length key))
               (setq returned (funcall condition))
               (forward-char (length key))
               returned)))
    (delete-char (- (length key)))
    (if (functionp expansion)
        (call-interactively expansion)
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

(defun als-set-expanding-ligatures (keymap &rest rest)
  "Set multiple KEY-EXPANSIONS on KEYMAP for `als-set-expanding-ligature'.

Return the keymap.

The following keywords are avaliable:
  :cond CONDITION         set the condition for the the following snippets
  :cond-desc DESCRIPTION  set the description of the previous declared condition
  :desc                   set the description for the following snippet

For examples see the definition of `als-prefix-map'.

\(fn KEYMAP [:desc :cond :cond-desc] KEY-EXPANSIONS)"
  (let (item desc cond cond-desc)
    (while rest
      (setq item (pop rest))
      (if (keywordp item)
          (pcase item
            (:desc      (setq desc      (pop rest)))
            (:cond      (setq cond      (pop rest)))
            (:cond-desc (setq cond-desc (pop rest)))
            (_ (error "Unknown keyword: %s" item)))
        ;; regular key-expansion
        (let ((key item)
              (expansion (pop rest)))
          (als-make-prefix-map keymap key expansion cond))))))

(defun als-insert-subscript ()
  "Expansion function used for auto-subscript snippets."
  (interactive)
  (insert "_" (this-command-keys)))

(defun als-auto-index-condition ()
  "Condition usef for auto-subscript snippets."
  (and
   ;; Before is some indexable char
   (or (<= ?a (char-before) ?z)
       (<= ?A (char-before) ?Z))
   ;; Not a macro
   (not (and (save-excursion
               (and (search-backward "\\" (line-beginning-position) t)
                    (looking-at "\\\\[a-zA-Z0-9*@]+")))
             (<= (match-beginning 0) (point) (match-end 0))))
   ;; Inside math
   (texmathp)))

(defvar als-prefix-map
  (let ((keymap (make-sparse-keymap)))
    (als-set-expanding-ligatures
     keymap
     :cond #'texmathp
     "!=" 	"\\neq"
     "!>" 	"\\mapsto"
     "**" 	"\\cdot"
     "+-" 	"\\pm"
     "-+" 	"\\mp"
     "->" 	"\\to"
     "..." 	"\\dots"
     "<<" 	"\\ll"
     "<=" 	"\\leq"
     "<>" 	"\\diamond"
     "=<" 	"\\impliedby"
     "==" 	"&="
     "=>" 	"\\implies"
     ">=" 	"\\geq"
     ">>" 	"\\gg"
     "AA" 	"\\forall"
     "EE" 	"\\exists"
     "aii" 	"a_i"
     "aip1" 	"a_{i+1}"
     "cb" 	"^3"
     "iff" 	"\\iff"
     "inn" 	"\\in"
     "notin" 	"\\not\\in"
     "sr" 	"^2"
     "xx" 	"\\times"
     "|->" 	"\\mapsto"
     "||" 	"\\mid"
     "~=" 	"\\approx"
     "~~" 	"\\sim"
     ;; ".../" 	"\\frac{...}{}"
     ;; "...\\)a" 	"...\\) a"
     ;; "//" 	"\\frac{}{}"
     ;; "a" "+b 	a + b"
     ;; "a" "^ 	a^"
     ;; "a+" 	"a +"
     ;; "a." 	"\\dot{a}"
     ;; "a.." 	"\\ddot{a}"
     ;; "a^11" 	"a^{11}"
     ;; "a_11" 	"a_{11}"
     ;; "abar" 	"\\overline{a}"
     ;; "ahat" 	"\\hat{a}"
     ;; "a~" 	"\\tilde{a}"
     ;; "case" 	"cases env."
     ;; "part" 	"\\frac{\\partial }{\\partial }"
     ;; "pmat" 	"pmatrix"
     ;; "set" 	"\\{ \\}"
     ;; "sq" 	"\\sqrt{}"
     ;; "st" 	"\\text{s.t.}"
     ;; "v,." 	"\\vec{v}"
     ;;"\\\\\\"\\" 	"\\setminus"

     "arccos"  "\\arccos"
     "arccot"  "\\arccot"
     "arccot"  "\\arccot"
     "arccsc"  "\\arccsc"
     "arcsec"  "\\arcsec"
     "arcsin"  "\\arcsin"
     "arctan"  "\\arctan"
     "cos"  "\\cos"
     "cot"  "\\cot"
     "csc"  "\\csc"
     "exp"  "\\exp"
     "ln"  "\\ln"
     "log"  "\\log"
     "perp"  "\\perp"
     "sin"  "\\sin"
     "star"  "\\star"
     ;; "to"  "\\to"
     :cond #'als-auto-index-condition
     "0"  #'als-insert-subscript
     "1"  #'als-insert-subscript
     "2"  #'als-insert-subscript
     "3"  #'als-insert-subscript
     "4"  #'als-insert-subscript
     "5"  #'als-insert-subscript
     "6"  #'als-insert-subscript
     "7"  #'als-insert-subscript
     "8"  #'als-insert-subscript
     "9"  #'als-insert-subscript)
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

(defun als-reset-state ()
  "Reset snippet state.

Useful for tightening conditions for all snippets, to minimize accidental triggering."
  (setq als-current-prefix-map als-prefix-map))

;;;###autoload
(define-minor-mode auto-latex-snippets-mode
  "Minor mode for dynamically auto-expanding LaTeX snippets.

See TODO for the availible snippets."
  :init-value nil
  (if auto-latex-snippets-mode
      (progn
        (add-hook 'post-self-insert-hook #'als-post-self-insert-hook 0 t)
        (add-hook 'yas-before-expand-snippet-hook #'als-reset-state 0 t))
    (remove-hook 'post-self-insert-hook #'als-post-self-insert-hook t)))

(provide 'auto-latex-snippets)
;;; auto-latex-snippets.el ends here
