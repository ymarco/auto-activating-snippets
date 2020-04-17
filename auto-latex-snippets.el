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


(defun als-make-prefix-map (keymap key expansion)
  "Bind KEY as extended prefix in KEYMAP to EXPANTION.

EXPANTION must either be a string or function."
  (let ((incomplete-key (substring key 0 -1))
        (orig-keymap keymap))
    (mapc (lambda (c)
            (setq keymap (or (lookup-key keymap (string c))
                             (define-key keymap (string c) (make-sparse-keymap)))))
          incomplete-key)
    (define-key keymap (substring key -1)
      (cond ((stringp expansion)
             (lambda () (interactive)
               (delete-char (- (length key)))
               (insert expansion)))
            ((functionp expansion)
             expansion)
            (t
             (error "Expantion must be either a string or function"))))
    orig-keymap))

(defun als-set-expanding-ligatures (keymap key-expantions)
  "Set multiple expantions for `als-set-expanding-ligature'.

KEYMAP is passed to `als-set-expanding-ligature', and
KEY-EXPANTIONS should be an alist of (key . expantion)."
  (dolist (key-expansion key-expantions)
    (als-make-prefix-map keymap
                         (car key-expansion)
                         (cdr key-expansion))))



(defvar als-prefix-map
  (let ((keymap (make-sparse-keymap)))
    (als-set-expanding-ligatures
     keymap
     '(
       ("a1"       . "a_1")
       ("a_11"     . "a_{11}")
       ("a"        . "^ a^")
       ("a^11"     . "a^{11}")
       ("a+"       . "a +")
       ("a"        . "+b a + b")
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
       ("to"       . "\\to")
       ("->"       . "\\to")
       ("|->"      . "\\mapsto")
       ("!>"       . "\\mapsto")
       ("sin"      . "\\sin")
       ;; ("v,."      . "\\vec{v}")
       ;; ("abar"     . "\\overline{a}")
       ;; ("ahat"     . "\\hat{a}")
       ;; ("a~"       . "\\tilde{a}")
       ;; ("a."       . "\\dot{a}")
       ;; ("a.."      . "\\ddot{a}")
       ;; ("...\\)a"  . "...\\) a")
       ("\\\\\\" "\\setminus")
       ("pmat"     . "pmatrix")
       ("part"     . "\\frac{\\partial }{\\partial }")
       ;; ("sq"       . "\\sqrt{}")
       ("sr"       . "^2")
       ("cb"       . "^3")
       ("EE"       . "\\exists")
       ("AA"       . "\\forall")
       ("aii"      . "a_i")
       ;; ("aip1"     . "a_{i+1}")
       ;; ("set"      . "\\{ \\}")
       ("||"       . "\\mid")
       ("<>"       . "\\diamond")
       ("case"     . "cases env.")
       ;; ("st"       . "\\text{s.t.}")
       ("+-"       . "\\pm")
       ("-+"       . "\\mp")
       ;; ("nCr"      . "\\binom{n}{r}")
       ))
    keymap)
  "TODO.")

(defvar als-current-prefix-map als-prefix-map
  "TODO.")

(defun als-post-self-insert-hook ()
  "TODO."
  (let ((k (lookup-key als-current-prefix-map (this-command-keys))))
    (setq als-current-prefix-map
          (cond ((null k)
                 als-prefix-map)
                ((functionp k)
                 (funcall k)
                 als-prefix-map)
                (t ;; (keymapp k)
                 k)))))



;;;###autoload
(define-minor-mode auto-latex-snippets-mode
  "TODO."
  :init-value nil
  (if auto-latex-snippets-mode
      (add-hook 'post-self-insert-hook #'als-post-self-insert-hook)
    (remove-hook 'post-self-insert-hook #'als-post-self-insert-hook)))

(provide 'auto-latex-snippets)
;;; auto-latex-snippets.el ends here
