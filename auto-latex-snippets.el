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

(defun als-current-snippet-insert-post-space-if-wanted ()
  (when (and (stringp als-transient-snippet-expansion)
             (= ?\\ (aref als-transient-snippet-expansion 0))
             (not (memq (char-after) '(?\) ?\]))))
    (insert " ")))

(add-hook 'als-post-snippet-expand-hook #'als-current-snippet-insert-post-space-if-wanted)

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

(defun als-insert-script (s)
  "Add a subscript with a text of S (string).

Rely on `als-transient-snippet-condition-result' to contain the
result of `als-auto-script-condition' which gives the info
whether to extend an existing subscript (e.g a_1 -> a_{1n}) or
insert a new subscript (e.g a -> a_1)."
  (interactive (list (this-command-keys)))
  (pcase als-transient-snippet-condition-result
    ;; new subscript after a letter
    ('one-sub
     (insert "_" s))
    ;; continuing a digit sub/superscript
    ('extended-sub
     (backward-char)
     (insert "{")
     (forward-char)
     (insert s "}"))))

(defun als-auto-script-condition ()
  "Condition used for auto-sub/superscript snippets."
  (cond ((and (or (= (char-before (1- (point))) ?_)
                  (= (char-before (1- (point))) ?^))
              (/= (char-before) ?{))
         'extended-sub)
        ((and
          ;; Before is some indexable char
          (or (<= ?a (char-before) ?z)
              (<= ?A (char-before) ?Z))
          ;; Before that is not
          (not (or (<= ?a (char-before (1- (point))) ?z)
                   (<= ?A (char-before (1- (point))) ?Z)))
          ;; Inside math
          (texmathp))
         'one-sub)))


(defun als-identify-adjacent-tex-object (&optional point)
  "Return the startig position of the left-adjacent TeX object from POINT."
  (save-excursion
    (goto-char (or point (point)))
    (cond
     ((memq (char-before) '(?\) ?\]))
      (backward-sexp)
      (point))
     ((and (= (char-before) ?})
           (save-excursion
             (cl-loop do (backward-sexp)
                      while (= (char-before) ?}))
             (looking-back "\\\\[A-Za-z@*]+" (line-beginning-position))))
      (match-beginning 0))
     ((or (<= ?a (char-before) ?z)
          (<= ?A (char-before) ?Z)
          (<= ?0 (char-before) ?9))
      (backward-word)
      (when (= (char-before) ?\\) (backward-char))
      (point)))))

(defun als-wrap-previous-object (tex-command)
  "Wrap previous TeX object in TEX-COMMAND."
  (interactive)
  (let ((start (als-identify-adjacent-tex-object)))
    (insert "}")
    (when (aref als-transient-snippet-key (1- (length als-transient-snippet-key)))
      (insert " "))
    (save-excursion
      (goto-char start)
      (insert (concat "\\" tex-command "{")))))

(defun als-object-on-left-condition ()
  "Return t if there is a TeX object imidiately to the left."
  ;; TODO use `als-identify-adjacent-tex-object'
  (and (or (<= ?a (char-before) ?z)
           (<= ?A (char-before) ?Z)
           (<= ?0 (char-before) ?9)
           (memq (char-before) '(?\) ?\] ?})))
       (texmathp)))

;; HACK smartparens runs after us on the global `post-self-insert-hook' and
;;      thinks that because a { was inserted after a self-insert event that it
;;      should insert the matching } even though we took care of that.
;; TODO check it without `smartparens-global-mode' as well
(defun als--shut-up-smartparens ()
  "Make sure `smartparens' doesn't mess up after snippet expanding to parentheses."
  (when (bound-and-true-p smartparens-mode)
    (let ((gpsih (default-value 'post-self-insert-hook)))
      (setq-default post-self-insert-hook nil)
      ;; push rather than add-hook so it doesn't run right after this very own
      ;; hook, but next time
      (push (lambda ()
              (setq-default post-self-insert-hook gpsih))
            post-self-insert-hook))))

(defun als-smart-fraction ()
  "Expansion function used for auto-subscript snippets."
  (interactive)
  (let* ((tex-obj (als-identify-adjacent-tex-object))
         (start (save-excursion
                  ;; if bracketed, delete outermost brackets
                  (if (memq (char-before) '(?\) ?\]))
                      (progn
                        (backward-delete-char 1)
                        (goto-char tex-obj)
                        (delete-char 1))
                    (goto-char tex-obj))
                  (point)))
         (end (point))
         (content (buffer-substring-no-properties start end)))
    (yas-expand-snippet (format "\\frac{%s}{$1}$0" content)
                        start end))
  (als--shut-up-smartparens))

(defvar als-basic-snippets
  (list
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
   ;; "...\\)a" 	"...\\) a"
   ;; "//" 	"\\frac{}{}"
   ;; "a" "+b 	a + b"
   ;; "a" "^ 	a^"
   ;; "a+" 	"a +"
   ;; "a^11" 	"a^{11}"
   ;; "case" 	"cases env."
   ;; "part" 	"\\frac{\\partial }{\\partial }"
   ;; "pmat" 	"pmatrix"
   ;; "set" 	"\\{ \\}"
   ;; "sq" 	"\\sqrt{}"
   ;; "st" 	"\\text{s.t.}"
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
   "gcd"   "\\gcd"

   "CC" "\\CC"
   "FF" "\\FF"
   "HH" "\\HH"
   "NN" "\\NN"
   "PP" "\\PP"
   "QQ" "\\QQ"
   "RR" "\\RR"
   "ZZ" "\\ZZ"

   ";a"  "\\alpha"
   ";A"  "\\forall"        ";;A" "\\aleph"
   ";b"  "\\beta"
   ";;;c" "\\cos"
   ";;;C" "\\arccos"
   ";d"  "\\delta"         ";;d" "\\partial"
   ";D"  "\\Delta"         ";;D" "\\nabla"
   ";e"  "\\epsilon"       ";;e" "\\varepsilon"   ";;;e" "\\exp"
   ";E"  "\\exists"                               ";;;E" "\\ln"
   ";f"  "\\phi"           ";;f" "\\varphi"
   ";F"  "\\Phi"
   ";g"  "\\gamma"                                ";;;g" "\\lg"
   ";G"  "\\Gamma"                                ";;;G" "10^{?}"
   ";h"  "\\eta"           ";;h" "\\hbar"
   ";i"  "\\in"            ";;i" "\\imath"
   ";I"  "\\iota"          ";;I" "\\Im"
   ";;j" "\\jmath"
   ";k"  "\\kappa"
   ";l"  "\\lambda"        ";;l" "\\ell"          ";;;l" "\\log"
   ";L"  "\\Lambda"
   ";m"  "\\mu"
   ";n"  "\\nu"                                   ";;;n" "\\ln"
   ";N"  "\\nabla"                                ";;;N" "\\exp"
   ";o"  "\\omega"
   ";O"  "\\Omega"         ";;O" "\\mho"
   ";p"  "\\pi"            ";;p" "\\varpi"
   ";P"  "\\Pi"
   ";q"  "\\theta"         ";;q" "\\vartheta"
   ";Q"  "\\Theta"
   ";r"  "\\rho"           ";;r" "\\varrho"
   ";;R" "\\Re"
   ";s"  "\\sigma"         ";;s" "\\varsigma"    ";;;s" "\\sin"
   ";S"  "\\Sigma"                               ";;;S" "\\arcsin"
   ";t"  "\\tau"                                 ";;;t" "\\tan"
   ";;;T" "\\arctan"
   ";u"  "\\upsilon"
   ";U"  "\\Upsilon"
   ";v"  "\\vee"
   ";V"  "\\Phi"
   ";w"  "\\xi"
   ";W"  "\\Xi"
   ";x"  "\\chi"
   ";y"  "\\psi"
   ";Y"  "\\Psi"
   ";z"  "\\zeta"
   ";0"  "\\emptyset"
   ";8"  "\\infty"
   ";!"  "\\neg"
   ";^"  "\\uparrow"
   ";&"  "\\wedge"
   ";~"  "\\approx"        ";;~" "\\simeq"
   ";_"  "\\downarrow"
   ";+"  "\\cup"
   ";-"  "\\leftrightarrow"";;-" "\\longleftrightarrow"
   ";*"  "\\times"
   ";/"  "\\not"
   ";|"  "\\mapsto"        ";;|" "\\longmapsto"
   ";\\" "\\setminus"
   ";="  "\\Leftrightarrow"";;=" "\\Longleftrightarrow"
   ";(" "\\langle"
   ";)" "\\rangle"
   ";[" "\\Leftarrow"     ";;[" "\\Longleftarrow"
   ";]" "\\Rightarrow"    ";;]" "\\Longrightarrow"
   ";{"  "\\subset"
   ";}"  "\\supset"
   ";<"  "\\leftarrow"    ";;<" "\\longleftarrow"  ";;;<" "\\min"
   ";>"  "\\rightarrow"   ";;>" "\\longrightarrow" ";;;>" "\\max"
   ";'"  "\\prime"
   ";."  "\\cdot")
  "Basic snippets. Expand only inside maths.")

(defvar als-subscript-snippets
  (nconc (list
          :cond #'als-auto-script-condition)
         (cl-loop for (key . exp) in '(("ii"  . als-insert-script)
                                       ("ip1" . "_{i+1}")
                                       ("jj"  . als-insert-script)
                                       ("jp1" . "_{j+1}")
                                       ("nn"  . als-insert-script)
                                       ("np1" . "_{n+1}")
                                       ("kk"  . als-insert-script)
                                       ("kp1" . "_{k+1}")
                                       ("0"   . als-insert-script)
                                       ("1"   . als-insert-script)
                                       ("2"   . als-insert-script)
                                       ("3"   . als-insert-script)
                                       ("4"   . als-insert-script)
                                       ("5"   . als-insert-script)
                                       ("6"   . als-insert-script)
                                       ("7"   . als-insert-script)
                                       ("8"   . als-insert-script)
                                       ("9"   . als-insert-script))
                  if (symbolp exp)
                  collect :expansion-desc
                  and collect (format "_%s, or _{X%s} if a subscript was typed already"
                                      (substring key -1) (substring key -1))
                  collect key collect exp))
  "Automatic subscripts! Expand In math and after a single letter.")

(defvar als-frac-snippet
  (list
   :cond #'als-object-on-left-condition
   :expansion-desc "Wrap object on the left with \\frac{}{}, leave `point' in the denuminator."
   "/" #'als-smart-fraction)
  "Frac snippet. Expand in maths when there's something to frac on on the left.")


(defvar als-accent-snippets
  `(:cond ,#'als-object-on-left-condition
    .
    ,(cl-loop for (key . exp) in '((". " . "dot")
                                  (".. " . "dot")
                                  (",." . "vec")
                                  (".," . "vec")
                                  ("~ " . "tilde")
                                  ("hat" . "hat")
                                  ("bar" . "overline"))
             collect :expansion-desc
             collect (format "Wrap in \\%s{}" exp)
             collect key
             ;; re-bind exp so its not changed in the next iteration
             collect (let ((expp exp)) (lambda () (interactive) (als-wrap-previous-object expp)))))
  "A simpler way to apply accents. Expand If LaTeX symbol immidiately before point.")

(defvar als-prefix-map
  (let ((keymap (make-sparse-keymap)))
    (apply #'als-set-snippets keymap als-basic-snippets)
    (apply #'als-set-snippets keymap als-subscript-snippets)
    (apply #'als-set-snippets keymap als-frac-snippet)
    (apply #'als-set-snippets keymap als-accent-snippets)
    keymap)
  "Defalut snippet keymap.")

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
  "TODO. THING."
  (replace-regexp-in-string
   "`\\|'" "~"
   (or (get thing 'variable-documentation)
       (documentation thing))))

(defun als--format-snippet-array (snippets)
  "TODO. SNIPPETS."
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
