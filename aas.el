;;; aas.el --- Snippet expansions mid-typing -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020-2021 Yoav Marco
;;
;; Author: Yoav Marco <yoavm448@gmail.com>
;; Maintainer: Yoav Marco <yoavm448@gmail.com>
;; Created: April 17, 2020
;; Modified: February 20, 2022
;; Version: 1.2
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

(declare-function yas-expand-snippet "yasnippet")
(declare-function tempel-insert "tempel")
(declare-function embark-completing-read-prompter "embark")

(defgroup aas nil
  "Snippet expansions mid-typing."
  :link '(url-link :tag "Homepage" "https://github.com/ymarco/auto-activating-snippets")
  :link '(emacs-library-link :tag "Library Source" "aas.el")
  :group 'abbrev
  :group 'tools
  :group 'matching
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
  "List of symbols of the active keymaps.

Each symbol should be a valid key in the hash table `aas-keymaps'.")

(defun aas-expand-snippet-maybe (key expansion &optional condition)
  "Try to expand snippet with KEY to EXPANSION.

Confirm first that KEY in its entirety is present before `point'.
If CONDITION is a function, call it (from the position in the
buffer exactly before the key) and do not expand if it returned
nil. Otherwise CONDITION is ignored. If all of these conditions
are valid, expand the snippet and return t. Otherwise return nil.

CONDITION should not modify the buffer when called.

EXPANSION can be either:
- A string
- A function, which would be called interactively
- A list with `yas' at the start, in which case the expansion
  will call `yas-expand-snippet' on the rest of the list
- A list with `tempel' at the start, in which case it will call
  `tempel-insert'."
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
    (undo-boundary)
    (delete-char (- (length key)))
    (run-hooks 'aas-pre-snippet-expand-hook)
    (pcase aas-transient-snippet-expansion
      ((and (pred stringp) s)
       (insert s))
      ((and (pred functionp) f)
       (call-interactively f))
      (`(tempel . ,tsnip)
       (tempel-insert tsnip))
      (`(yas . ,ysnip)
       (apply #'yas-expand-snippet ysnip))
      (bad (error "Invalid AAS expansion form: %S" bad)))
    (run-hooks 'aas-post-snippet-expand-hook)
    t))

(defun aas-define-prefix-map-snippet (keymap key expansion &optional condition)
  "Bind KEY (string) as extended prefix in KEYMAP to EXPANTION.

EXPANSION can be either:
- A string
- A function, which would be called interactively
- A list with `yas' at the start, in which case the expansion
  will call `yas-expand-snippet' on the rest of the list
- A list with `tempel' at the start, in which case it will call
CONDITION must be nil or a function."
  (unless (or (stringp expansion) (functionp expansion) (null expansion)
              (and (consp expansion) (memq (car expansion) `(tempel yas))))
    (error "Expansion must be either a string, function, tempel/yas form, or nil"))
  (unless (or (null condition) (functionp condition))
    (error "Condition must be either nil or a function"))
  (define-key keymap key
    (when expansion
      (lambda () (aas-expand-snippet-maybe key expansion condition)))))

(defun aas-set-snippets (name &rest args)
  "Define snippets for the keymap named NAME (usually a major or minor-mode name).

Later, the keymap can be activated by calling `aas-activate-keymap' with NAME.

Example call:
\(aas-set-snippets 'org-mode
   :cond #'bolp
   \"#+lh\" \"#+latex_header: \"
   \"#+hh\" \"#+html_header: \"
   \"#+title\" (lambda () (interactive)
               (insert \"#+title: \" user-full-name)))

Specification:

KEY-EXPANSIONS is a plist of snippet keys and their expansions.
keys must be strings, and expansions must be of these types:
- String, meaning the key would be replaced by the expansion
  string.
- Function, meaning the key would be removed and the function
  would be called interactively to modify the buffer.
- nil, meaning expansion for the key is disabled.

Additionally, a sequence of :cond FN can be inserted between
key-expansion pairs. This would make all the snippets writen
after the :cond first call FN (non-interactively), and only
expand if it returned non-nil. To remove a previously-set
condition, use :cond nil.

During the expansion process, user-provided functions for
conditions and expansions are free to use the variables
`aas-transient-snippet-key', `aas-transient-snippet-expansion',
`aas-transient-snippet-condition-result', which see.

\(fn KEYMAP [:cond :expansion-desc] KEY-EXPANSIONS)"
  (declare (indent 1))
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
            ;; Make sure the loop progresses even in the face of odd output from
            ;; `this-command-keys'
            (t (cl-callf cdr current-map-sublist)
               (setcdr prev current-map-sublist))))))

;;;###autoload
(defun aas-activate-keymap (keymap-symbol)
  "Add KEYMAP-SYMBOL to the list of active snippet keymaps.

Return non-nil if that keymap actually exists and was added."
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
  :group 'aas
  (if aas-mode
      (add-hook 'post-self-insert-hook #'aas-post-self-insert-hook 0 t)
    (remove-hook 'post-self-insert-hook #'aas-post-self-insert-hook t)))

;;;###autoload
(define-global-minor-mode aas-global-mode aas-mode
  (lambda ()
    (aas-mode +1)
    (aas-activate-keymap 'global)))
;; define-global-minor-mode doesn't let us write documentation
(put
 'aas-global-mode 'function-documentation
 "Global `aas-mode'. The activated keymap is `global': set global snippets with
\(aas-set-snippets 'global ...)")

;;;###autoload
(defun aas-activate-for-major-mode ()
  "Activate the aas keymap for `major-mode' and all its ancestor modes."
  (aas-mode +1)
  (mapc #'aas-activate-keymap (aas--modes-to-activate major-mode)))
;; The function had a typo aas->ass that was only found recently.
;; Try not to break people's configurations with the typo fixed
(define-obsolete-function-alias
  'ass-activate-for-major-mode #'aas-activate-for-major-mode
  "1.1" "This was a horrible typo of `aas-activate-for-major-mode', but it
appeared in the readme for months.")

(defun aas-embark-menu ()
  (interactive)
  (when-let (command (embark-completing-read-prompter
                      aas--prefix-map nil 'no-default))
    (call-interactively command)))

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
