;;; aas.el --- Snippet expansions mid-typing -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020-2021 Yoav Marco
;;
;; Author: Yoav Marco <yoavm448@gmail.com>
;; Maintainer: Yoav Marco <yoavm448@gmail.com>
;; Created: April 17, 2020
;; Modified: April 17, 2021
;; Version: 1.0
;; Keywords: abbrev, tools
;; Homepage: https://github.com/ymarco/auto-activating-snippets
;; Package-Requires: ((emacs "26.3"))
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; This package implements an engine for auto-expanding snippets.
;; It is done by tracking your inputted chars along a tree until you
;; complete a registered key sequence.
;;
;; Its like running a long prefix command, but the keys you type are not
;; 'consumed' and appear in the buffer until you complete the whole command
;; --- and then the snippet is triggered!
;;
;;; Code:

(require 'cl-lib)

(defgroup aas nil
  "Snippet expansions mid-typing."
  :prefix "aas-")

(defcustom aas-pre-snippet-expand-hook nil
  "Hooks to run just before expanding snippets."
  :type 'hook
  :group 'aas)
(defcustom aas-post-snippet-expand-hook nil
  "Hooks to run just after expanding snippets."
  :type 'hook
  :group 'aas)

(defvar-local aas-transient-snippet-key nil
  "Key of the active snippet.
Defined while calling the expansion and condition functions, and
during evaluation of `aas-pre-snippet-expand-hook' and
`aas-post-snippet-expand-hook'.")
(defvar-local aas-transient-snippet-expansion nil
  "Expansion of the active snippet.
Defined while calling the expansion and condition functions, and
during evaluation of `aas-pre-snippet-expand-hook' and
`aas-post-snippet-expand-hook'.")
(defvar-local aas-transient-snippet-condition-result nil
  "Result of the condition that was run for the active snippet.
Defined while calling the expansion function, and during
evaluation of `aas-pre-snippet-expand-hook' and
`aas-post-snippet-expand-hook'.")

(defun aas--key-is-fully-typed? ()
  "Check if `aas-transient-snippet-key' in its entirety is proceeding `point'."
  (save-excursion
    (search-forward aas-transient-snippet-key
                    (+ (point) (length aas-transient-snippet-key))
                    t)))

(defcustom aas-global-condition-hook nil
  "A list of conditions to run before each expansion.
If any evaluate to nil, do not expand the snippet."
  :type 'hook
  :group 'aas)
(add-hook 'aas-global-condition-hook #'aas--key-is-fully-typed?)

(defvar aas-keymaps (make-hash-table :test #'eq)
  "Hash table of all snippet keymaps, in the format of symbol:keymap.")

(defvar-local aas-active-keymaps nil
  "List of symbols of the active keymaps. Each symbol should be
present as a key in `aas-keymaps'.")

(defun aas-expand-snippet-maybe (key expansion &optional condition)
  "Try to expand snippet with KEY to EXPANSION.

Confirm first that KEY in its entirety is present before `point'.
If CONDITION is a function, call it (from the position in the
buffer exactly before the key) and do not expand if it returned
nil. Otherwise CONDITION is ignored. If all of these conditions
are valid, expand the snippet and return t. Otherwise return nil.

CONDITION should not modify the buffer when called.

EXPANTION is called interactively, and CONDITION
non-interactively."
  (when-let ((aas-transient-snippet-key key)
             (aas-transient-snippet-expansion expansion)
             (aas-transient-snippet-condition-result
              (progn
                (backward-char (length key)) ; call conditions with point *before* key
                (prog1 (and
                        ;; global conditions
                        (run-hook-with-args-until-failure 'aas-global-condition-hook)
                        ;; snippet-specific condition
                        (or (null condition)
                            (funcall condition)))
                  ;; go back to after the key
                  (forward-char (length key))))))
    (delete-char (- (length key)))
    (run-hooks 'aas-pre-snippet-expand-hook)
    (if (functionp expansion)
        (call-interactively expansion)
      (insert expansion))
    (run-hooks 'aas-post-snippet-expand-hook)
    t))

(defun aas-define-prefix-map-snippet (keymap key expansion &optional condition)
  "Bind KEY (string) as extended prefix in KEYMAP to EXPANTION.

EXPANTION must either be a string, an interactive function, or nil.
CONDITION must be nil or a function."
  (unless (or (stringp expansion) (functionp expansion) (null expansion))
    (error "Expansion must be either a string, function, or nil"))
  (unless (or (null condition) (functionp condition))
    (error "Condition must be either nil or a function"))
  (define-key keymap key
    (when expansion
      (lambda () (aas-expand-snippet-maybe key expansion condition)))))

(defun aas-set-snippets (name &rest args)
  "Define snippets for NAME (a symbol entry to `aas-keymaps').

NAME should be later used in `aas-activate-keymap' and such.

The following keywords in ARGS are avaliable:
  :cond CONDITION         set the condition for the the following snippets

For examples see the definition of `aas--prefix-map'.

\(fn KEYMAP [:cond :expansion-desc] KEY-EXPANSIONS)"
  (let ((keymap (or (gethash name aas-keymaps) (make-sparse-keymap)))
        item cond)
    (while args
      (setq item (pop args))
      (if (keywordp item)
          (pcase item
            (:cond (setq cond (pop args)))
            ;; ignoring this
            (:expansion-desc  (pop args))
            (_ (error "Unknown keyword: %s" item)))
        ;; regular key-expansion
        (let ((key item)
              (expansion (pop args)))
          (aas-define-prefix-map-snippet keymap key expansion cond))))
    (puthash name keymap aas-keymaps)))

(defvar-local aas--prefix-map nil
  "Defalut full snippet keymap.")

(defvar-local aas--current-prefix-maps (list nil)
  "Global variable to keep track of the current user path trace of snippets.

A list of active keymap trees that may result in expansion, with
the first element always nil for update logic simplicity.

Gets updated by `aas-post-self-insert-hook'.")

(defun aas-post-self-insert-hook ()
  "Try to expand snippets automatically.

Use for the typing history, `aas--current-prefix-maps' and
`this-command-keys' for the current typed key.."
  (cl-callf nconc (cdr aas--current-prefix-maps) (list aas--prefix-map))
  (let ((current-map-sublist (cdr aas--current-prefix-maps))
        (prev aas--current-prefix-maps)
        current-map
        key-result)
    (while current-map-sublist
      (setq current-map (car current-map-sublist)
            key-result (lookup-key current-map (this-command-keys)))
      (cond ((null key-result)
             ;; remove dead end from the list
             (cl-callf cdr current-map-sublist)
             (setcdr prev current-map-sublist))
            ((keymapp key-result)
             ;; update tree
             (setcar current-map-sublist key-result)
             (setq prev current-map-sublist)
             (cl-callf cdr current-map-sublist))
            ((functionp key-result)
             ;; an ending! no need to call interactively,`aas-expand-snippet-maybe'
             ;; takes care of that
             (if (funcall key-result)
                 ;; condition evaluated to true, and snippet expanded!
                 (setq current-map-sublist nil      ; stop the loop
                       aas--current-prefix-maps (list nil)) ; abort all other snippet
               ;; unseccesfull. remove dead end from the list
               (cl-callf cdr current-map-sublist)
               (setcdr prev current-map-sublist)))
            ;; Make sure the loop progress even in the face of objectionable output from
            ;; (this-command-keys)
            (t (cl-callf cdr current-map-sublist)
               (setcdr prev current-map-sublist))))))

;;;###autoload
(defun aas-activate-keymap (keymap-symbol)
  "Add KEYMAP-SYMBOL to the list of active snippet keymaps.

Return non-nil if that keymap actually exists and was added.
Otherwise return nil."
  (when (gethash keymap-symbol aas-keymaps)
    (add-to-list 'aas-active-keymaps keymap-symbol)
    (setq aas--prefix-map (make-composed-keymap
                           (mapcar (lambda (x) (gethash x aas-keymaps))
                                   aas-active-keymaps)))))

(defun aas-deactivate-keymap (keymap-symbol)
  "Remove KEYMAP-SYMBOL from the list of active keymaps."
  (cl-callf2 delq keymap-symbol aas-active-keymaps)
  (setq aas--prefix-map (make-composed-keymap
                         (mapcar (lambda (x) (gethash x aas-keymaps))
                                 aas-active-keymaps))))

(defun aas--modes-to-activate (mode)
  "Return the list of ancestors for MODE.
\(aas--modes-to-activate 'org-mode)  => (text-mode outline-mode org-mode)"
  (cl-loop for parent = mode
           then (or (get parent 'derived-mode-parent)
                    ;; if parent is an alias, continue with the aliased
                    (let ((aliased? (symbol-function parent)))
                      (and (symbolp aliased?) aliased?)))
           while parent
           collect parent into modes
           finally return (nreverse modes)))

;;;###autoload
(define-minor-mode aas-mode
  "Minor mode for dynamically auto-expanding snippets.

This does not set any default keymaps. For that use
`aas-activate-for-major-mode' and `aas-activate-keymap'."
  :init-value nil
  (if aas-mode
      (add-hook 'post-self-insert-hook #'aas-post-self-insert-hook 0 t)
    (remove-hook 'post-self-insert-hook #'aas-post-self-insert-hook t)))

;;;###autoload
(defun aas-activate-for-major-mode ()
  (aas-mode +1)
  (mapc #'aas-activate-keymap (aas--modes-to-activate major-mode)))
;; The function had a typo aas->ass that was only found recently.
;; Try not to break people's configurations with the typo fixed
(define-obsolete-function-alias
  'ass-activate-for-major-mode #'aas-activate-for-major-mode
  "1.1" "This was a horrible typo of `aas-activate-for-major-mode', but it
appeared in the readme for months.")

(defun aas--format-doc-to-org (thing)
  "Format documentation of THING in `org-mode' syntax."
  (replace-regexp-in-string
   "`\\|'" "~"
   (or (get thing 'variable-documentation)
       (documentation thing))))

(defun aas--format-snippet-array (snippets)
  "Format SNIPPETS to a 2D list of key-expansion.

SNIPPETS should resemble an input to `aas-set-snippets'."
  (let (item expansion-desc res)
    (while snippets
      (setq item (pop snippets))
      (if (keywordp item)
          (pcase item
            (:expansion-desc  (setq expansion-desc (pop snippets)))
            ;; ignore, assume the condition is recorded in the doscring of the
            ;; var dolding this list
            (:cond                                 (pop snippets))
            (_ (error "Unknown keyword: %s" item)))
        (let ((key item)
              (expansion (pop snippets)))
          (push
           (list
            (format
             "~%s~"
             ;; escape org mode syntax in snippet keys
             (replace-regexp-in-string
              " " "␣"
              (replace-regexp-in-string
               "~" "∽"
               (replace-regexp-in-string "|" "❘" key))))
            (or expansion-desc
                ;; just to be clear
                expansion))
           res))
        ;; expasion-desc is one per snippet
        (setq expansion-desc nil)))
    (nreverse res)))

(provide 'aas)
;;; aas.el ends here
