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
;; Package-Requires: ((emacs 26.1) (cl-lib "0.5") (yasnippet 0.14))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  automatic expansion of latex macros
;;
;;; Code:

(require 'cl-lib)
(require 'texmathp)
(require 'yasnippet)

(defvar als-pre-snippet-expand-hook nil
  "Hooks to run just before expanding snippets.")
(defvar als-post-snippet-expand-hook nil
  "Hooks to run just after expanding snippets.")

(defvar als-transient-snippet-key nil
  "KEY of the active snippet, defined while calling the expansion and condition functions, as well as `als-pre-snippet-expand-hook' and `als-post-snippet-expand-hook'.")
(defvar als-transient-snippet-expansion nil
  "EXPANSION of the active snippet, defined while calling the expansion and condition functions, as well as `als-pre-snippet-expand-hook' and `als-post-snippet-expand-hook'.")
(defvar als-transient-snippet-condition-result nil
  "Result of CONDITION of the active snippet, defined while calling the expansion and condition functions, as well as `als-pre-snippet-expand-hook' and `als-post-snippet-expand-hook'.")

(defun als-expand-snippet-maybe (key expansion &optional condition)
  "Expand snippet with KEY as EXPANSION.

When CONDITION is a function, call it (from the position in the
buffer exactly before the key) and do not expand if it returned
nil. CONDITION is expected not to modify the buffer.

EXPANTION is called interactively, and CONDITION
non-interactively."
  (when-let ((als-transient-snippet-key key)
             (als-transient-snippet-expansion expansion)
             (als-transient-snippet-condition-result
              (and
               ;; key was fully typed
               (save-excursion
                 (search-backward key (- (point) (length key)) t))
               ;; condition is either not present, or evaluates to true
               (or (null condition)
                   (backward-char (length key))
                   (prog1 (funcall condition)
                     (forward-char (length key)))))))
    (delete-char (- (length key)))
    (run-hooks 'als-pre-snippet-expand-hook)
    (if (functionp expansion)
        (call-interactively expansion)
      (insert expansion))
    (run-hooks 'als-post-snippet-expand-hook)
    t))



(defun als-define-prefix-map-snippet (keymap key expansion &optional condition)
  "Bind KEY (string) as extended prefix in KEYMAP (keymap) to EXPANTION.

EXPANTION must either be a string, an interactive function, or nil.
CONDITION must be nil or a function."
  (unless (or (stringp expansion) (functionp expansion) (null expansion))
    (error "Expansion must be either a string, function, or nil"))
  (unless (or (null condition) (functionp condition))
    (error "Condition must be either a string or function"))
  (define-key keymap key
    (when expansion
      (lambda () (als-expand-snippet-maybe key expansion condition)))))

(defun als-set-snippets (keymap &rest args)
  "Set multiple keys and expansions using KEYMAP as the tree to store in.

Return the keymap.

The following keywords in ARGS are avaliable:
  :cond CONDITION         set the condition for the the following snippets

For examples see the definition of `als-prefix-map'.

\(fn KEYMAP [:cond :expansion-desc] KEY-EXPANSIONS)"
  (let (item cond)
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
          (als-define-prefix-map-snippet keymap key expansion cond)))))
  keymap)


(defvar-local als-current-prefix-maps nil
  "Global variable to keep track of the current user path trace of snippets.

Gets updated by `als-post-self-insert-hook'.")

(defun als-post-self-insert-hook ()
  "Try to expand snippets automatically.

Use for the typing history, `als-current-prefix-maps' and
`this-command-keys' for the current typed key.."
  (setq als-current-prefix-maps (nconc als-current-prefix-maps (list als-prefix-map)))
  (let ((current-map-sublist als-current-prefix-maps)
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
               (setq als-current-prefix-maps (cdr als-current-prefix-maps))))
            ((keymapp key-result)
             ;; update tree
             (setcar current-map-sublist key-result))
            ((functionp key-result)
             ;; an ending! no need to call interactively,`als-expand-snippet-maybe'
             ;; takes care of that
             (if (funcall key-result)
                 ;; condition evaluated to true, and snipped expanded!
                 (setq current-map-sublist nil      ; stop the loop
                       als-current-prefix-maps nil) ; abort all other snippest
               ;; unseccesfull. remove dead end from the list
               (if prev
                   (setcdr prev (cdr current-map-sublist))
                 (setq als-current-prefix-maps (cdr als-current-prefix-maps))))))
      ;; proceed loop
      (setq prev current-map-sublist
            current-map-sublist (cdr-safe current-map-sublist)))))

(defun als--debug-print-tree-options ()
  "Print debug info about what entries into the tree are currently kept track of."
  (message "%s entries: %s"
           (length als-current-prefix-maps)
           (mapcar (lambda (kmap)
                     (apply #'string (sort (mapcar (lambda (key-and-binding)
                                                     (car key-and-binding))
                                                   (cdr kmap))
                                           #'<)))
                   als-current-prefix-maps)))

;;;###autoload
(define-minor-mode auto-latex-snippets-mode
  "Minor mode for dynamically auto-expanding LaTeX snippets.

See TODO for the availible snippets."
  :init-value nil
  (if auto-latex-snippets-mode
      (add-hook 'post-self-insert-hook #'als-post-self-insert-hook 0 t)
    (remove-hook 'post-self-insert-hook #'als-post-self-insert-hook t)))



(defun als--format-doc-to-org (thing)
  "Format documentation of THING in org-mode syntax."
  (replace-regexp-in-string
   "`\\|'" "~"
   (or (get thing 'variable-documentation)
       (documentation thing))))

(defun als--format-snippet-array (snippets)
  "Format SNIPPETS to a 2D list of key-expansion.

SNIPPETS should resemble an input to `als-set-snippets'."
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
           (list (replace-regexp-in-string
                  "|" "❘"
                  (replace-regexp-in-string " " "␣"  key))
                 (or expansion-desc
                     ;; just to be clear
                     expansion))
           res))
        ;; expasion-desc is one per snippet
        (setq expansion-desc nil)))
    (nreverse res)))

(provide 'auto-latex-snippets)
;;; auto-latex-snippets.el ends here
