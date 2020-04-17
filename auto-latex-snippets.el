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

(defun als-set-expanding-ligature (keymap key expansion)
  "Set KEY (a 2-char string) to expand into EXPANTION in KEYMAP."
  (define-key keymap (substring key 0 1)
    (lambda () (interactive)
      (insert (this-command-keys))
      (set-transient-map
       (let ((keymap (make-sparse-keymap)))
         (define-key keymap (substring key 1 2)
           (lambda () (interactive)
             (when (eq (char-before) (aref key 0)) ; sanity check
               (delete-char -1)
               (insert expansion))))
         keymap)))))

(defun als-set-expanding-ligatures (keymap key-expantions)
  "Set multiple expantions for `als-set-expanding-ligature'.

KEYMAP is passed to `als-set-expanding-ligature', and
KEY-EXPANTIONS should be an alist of (key . expantion)."
  (dolist (key-expansion key-expantions)
    (als-set-expanding-ligature keymap
                                (car key-expansion)
                                (cdr key-expansion))))



(defvar auto-latex-snippets-mode-map
  (let ((keymap (make-sparse-keymap)))
    (als-set-expanding-ligatures
     keymap
     '(("=>" . "\\implies")
       ("==" . "&=")
       ("~=" . "\\approx")
       ("~~" . "\\sim")
       ("~~" . "\\sim")
       (">=" . "\\geq")
       ("<=" . "\\leq")
       (">>" . "\\gg")
       ("<<" . "\\ll")
       ("xx" . "\\times")
       ("**" . "\\cdot")
       ;; ("to" . "\\to")
       ("->" . "\\to")
       ("+-" . "\\pm")
       ("-+" . "\\mp")))
    keymap)
  "TODO.")

;;;###autoload
(define-minor-mode auto-latex-snippets-mode
  "TODO."
  :init-value nil
  :keymap auto-latex-snippets-mode-map)

(provide 'auto-latex-snippets)
;;; auto-latex-snippets.el ends here
