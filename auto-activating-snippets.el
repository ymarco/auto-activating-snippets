;;; auto-activating-snippets.el --- Snippet expansions mid-typing -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020 Yoav Marco
;;
;; Author: Yoav Marco <http://github/yoavm448>
;; Maintainer: Yoav Marco <yoavm448@gmail.com>
;; Created: April 17, 2020
;; Modified: April 17, 2020
;; Version: 0.0.1
;; Homepage: https://github.com/tecosaur/auto-activating-snippets
;; Package-Requires: ((emacs "26.1") (dash "2.17.0"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  This package implements an engine for auto-expanding snippets.
;;  It is done by tracking your inputted chars along a tree until you
;;  complete a registered key sequence.
;;
;; Its like running a long prefix command, but the keys you type are not
;; 'consumed' and appear in the buffer until you complete the whole command
;; --- and then the snippet is triggered!
;;
;;; Code:

(require 'cl-lib)
(require 'dash)

(defvar aas-pre-snippet-expand-hook nil
  "Hooks to run just before expanding snippets.")
(defvar aas-post-snippet-expand-hook nil
  "Hooks to run just after expanding snippets.")

(defvar-local aas-transient-snippet-key nil
  "KEY of the active snippet, defined while calling the expansion and condition functions, as well as `aas-pre-snippet-expand-hook' and `aas-post-snippet-expand-hook'.")
(defvar-local aas-transient-snippet-expansion nil
  "EXPANSION of the active snippet, defined while calling the expansion and condition functions, as well as `aas-pre-snippet-expand-hook' and `aas-post-snippet-expand-hook'.")
(defvar-local aas-transient-snippet-condition-result nil
  "Result of CONDITION of the active snippet, defined while calling the expansion and condition functions, as well as `aas-pre-snippet-expand-hook' and `aas-post-snippet-expand-hook'.")

(defun aas--key-is-fully-typed? ()
  "Check if `aas-transient-snippet-key' in its entirety is proceeding `point'."
  (save-excursion
    (search-forward aas-transient-snippet-key
                    (+ (point) (length aas-transient-snippet-key))
                    t)))

(defcustom aas-global-condition-hook (list #'aas--key-is-fully-typed?)
  "A list of conditions to run before each expansion.
If any evaluate to non-nil, do not expand the snippet."
  :type 'hook
  :group 'aas)

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
    (error "Condition must be either a string or function"))
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

(defvar-local aas--current-prefix-maps nil
  "Global variable to keep track of the current user path trace of snippets.

Gets updated by `aas-post-self-insert-hook'.")

(defun aas-post-self-insert-hook ()
  "Try to expand snippets automatically.

Use for the typing history, `aas--current-prefix-maps' and
`this-command-keys' for the current typed key.."
  (cl-callf nconc aas--current-prefix-maps (list aas--prefix-map))
  (let ((current-map-sublist aas--current-prefix-maps)
        current-map
        key-result
        prev)
    (while current-map-sublist
      (setq current-map (car current-map-sublist)
            key-result (lookup-key current-map (this-command-keys)))
      (cond ((null key-result)
             ;; remove dead end from the list
             (if prev
                 (setcdr prev (cdr current-map-sublist))
               (cl-callf cdr aas--current-prefix-maps)))
            ((keymapp key-result)
             ;; update tree
             (setcar current-map-sublist key-result))
            ((functionp key-result)
             ;; an ending! no need to call interactively,`aas-expand-snippet-maybe'
             ;; takes care of that
             (if (funcall key-result)
                 ;; condition evaluated to true, and snipped expanded!
                 (setq current-map-sublist nil      ; stop the loop
                       aas--current-prefix-maps nil) ; abort all other snippest
               ;; unseccesfull. remove dead end from the list
               (if prev
                   (setcdr prev (cdr current-map-sublist))
                 (cl-callf cdr aas--current-prefix-maps)))))
      ;; proceed loop
      (setq prev current-map-sublist)
      (cl-callf cdr current-map-sublist))))

(defun aas--debug-print-tree-options ()
  "Print debug info about what entries into the tree are currently kept track of."
  (message "%s entries: %s"
           (length aas--current-prefix-maps)
           (mapcar (lambda (kmap)
                     (apply #'string (sort (mapcar (lambda (key-and-binding)
                                                     (car key-and-binding))
                                                   (cdr kmap))
                                           #'<)))
                   aas--current-prefix-maps)))

(defvar aas-keymaps (make-hash-table :test #'eq)
  "Hash table of all snippet keymaps, in the format of symbol:keymap.")

(defvar-local aas-active-keymaps nil
  "List of symbols of the active keymaps. Each symbol should be
present as a key in `aas-keymaps'.")

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
  (delq keymap-symbol aas-active-keymaps)
  (setq aas--prefix-map (make-composed-keymap
                         (mapcar (lambda (x) (gethash x aas-keymaps))
                                 aas-active-keymaps))))

;;;###autoload
(defun aas-activate-major-mode-snippets ()
  "Activate snippets for current `major-mode'."
  (aas-activate-keymap major-mode))


(defun aas--modes-to-activate (mode)
  "Return the list of ancestors for MODE.
\(aas--modes-to-activate 'org-mode)  => (text-mode outline-mode org-mode)"
  (let ((res nil))
    (while (not (eq mode 'fundamental-mode))
      (push mode res)
      (setq mode (or (get mode 'derived-mode-parent)
                     'fundamental-mode)))
    res))
;;;###autoload
(define-minor-mode auto-activating-snippets-mode
  "Minor mode for dynamically auto-expanding snippets."
  :init-value nil

  (if auto-activating-snippets-mode
      (progn
        (mapc #'aas-activate-keymap (aas--modes-to-activate major-mode))
        (add-hook 'post-self-insert-hook #'aas-post-self-insert-hook 0 t))
    (remove-hook 'post-self-insert-hook #'aas-post-self-insert-hook t)))

(defun aas--format-doc-to-org (thing)
  "Format documentation of THING in org-mode syntax."
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
           ;; replace | with unicode so org doesn't think its a table column
           (list
            (->> key
                 ;; escaping org mode syntax
                 (replace-regexp-in-string "|" "❘")
                 (replace-regexp-in-string "~" "∽")
                 (replace-regexp-in-string " " "␣"  )
                 (format "~%s~"))
            (or expansion-desc
                ;; just to be clear
                expansion))
           res))
        ;; expasion-desc is one per snippet
        (setq expansion-desc nil)))
    (nreverse res)))

(provide 'auto-activating-snippets)
;;; auto-activating-snippets.el ends here
