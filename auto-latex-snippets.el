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

When CONDITION is a function, call (from the position in the
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

EXPANTION must either be a string or an interactive function.
CONDITION must be nil or a function."
  (unless (or (stringp expansion) (functionp expansion))
    (error "Expansion must be either a string or function"))
  (unless (or (null condition) (functionp condition))
    (error "Condition must be either a string or function"))
  (define-key keymap key
    (lambda () (als-expand-snippet-maybe key expansion condition))))

(defun als-set-snippets (keymap &rest args)
  "Set multiple keys and expansions using KEYMAP as the tree to store in.

Return the keymap.

The following keywords in ARGS are avaliable:
  :cond CONDITION         set the condition for the the following snippets
  :cond-desc DESCRIPTION  set the description of the previously specified condition TODO
  :desc                   set the description for the following snippet TODO

For examples see the definition of `als-prefix-map'.

\(fn KEYMAP [:desc :cond :cond-desc] KEY-EXPANSIONS)"
  (let (item desc cond cond-desc)
    (while args
      (setq item (pop args))
      (if (keywordp item)
          (pcase item
            (:desc      (setq desc      (pop args)))
            (:cond      (setq cond      (pop args)))
            (:cond-desc (setq cond-desc (pop args)))
            (_ (error "Unknown keyword: %s" item)))
        ;; regular key-expansion
        (let ((key item)
              (expansion (pop args)))
          (als-define-prefix-map-snippet keymap key expansion cond)))))
  keymap)

(defun als-insert-script ()
  "Expansion function used for auto-sub/superscript snippets."
  (interactive)
  (pcase als-transient-snippet-condition-result
   ;; new subscript after a letter
   ('one-sub
    (insert "_" (this-command-keys)))
   ;; continuing a digit sub/superscript
   ('extended-sub
    (backward-char)
    (insert "{")
    (forward-char)
    (insert (this-command-keys) "}"))))

(defun als-auto-script-condition ()
  "Condition used for auto-sub/superscript snippets."
  (cond ((and
          ;; Before is some indexable char
          (or (<= ?a (char-before) ?z)
              (<= ?A (char-before) ?Z))
          ;; Before that is not
          (not (or (<= ?a (char-before (1- (point))) ?z)
                   (<= ?A (char-before (1- (point))) ?Z)))
          ;; Inside math
          (texmathp))
         'one-sub)
        ((and
          ;; Before is another digit subscript/superscript
          (<= ?0 (char-before) ?9)
          (or (= (char-before (1- (point))) ?_)
              (= (char-before (1- (point))) ?^))
          (texmathp))
         'extended-sub)))


(defun als-identify-adjacent-tex-object (&optional point)
  "Return the startig position of the left-adjacent TeX object from POINT."
  (save-excursion
    (goto-char (or point (point)))
    (cond
     ((memq (char-before) '(?\) ?\]))
      (backward-sexp)
      (point))
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
  (and (or (<= ?a (char-before) ?z)
           (<= ?A (char-before) ?Z)
           (<= ?0 (char-before) ?9)
           (memq (char-before) '(?\) ?\])))
       (texmathp)))

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
  ;; HACK smartparens runs after us on the global `post-self-insert-hook' and
  ;;      thinks that because a { was inserted after a self-insert event that it
  ;;      should insert the matching } even though we took care of that.
  ;; TODO check it without `smartparens-global-mode' as well
  (when (bound-and-true-p smartparens-mode)
    (let ((gpsih (default-value 'post-self-insert-hook)))
      (setq-default post-self-insert-hook nil)
      ;; push rather than add-hook so it doesn't run right after this very own
      ;; hook, but next time
      (push (lambda ()
              (setq-default post-self-insert-hook gpsih))
            post-self-insert-hook))))

(defvar als-default-snippets
  (list
   :cond #'texmathp
   :cond-desc "inside math"
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
   ";."  "\\cdot"

   :cond #'als-object-on-left-condition
   :cond-desc "If LaTeX object immidiately to the left"
   :desc "Smart fraction"
   "/" #'als-smart-fraction

   ;; "to"  "\\to"
   :cond #'als-auto-script-condition
   :cond-desc "In math and after a single letter"
   :desc "Automatic subscripts"
   "ii"  "_i"
   "ip1" "_{i+1}"
   "jj"  "_j"
   "jp1" "_{j+1}"
   "nn"  "_n"
   "np1" "_{n+1}"
   "kk"  "_k"
   "kp1" "_{k+1}"
   "0"   #'als-insert-script
   "1"   #'als-insert-script
   "2"   #'als-insert-script
   "3"   #'als-insert-script
   "4"   #'als-insert-script
   "5"   #'als-insert-script
   "6"   #'als-insert-script
   "7"   #'als-insert-script
   "8"   #'als-insert-script
   "9"   #'als-insert-script

    ;; accents
    :cond #'als-object-on-left-condition
    :cond-desc "If LaTeX symbol immidiately before point."
    :desc "A simpler way to apply accents"
    ". "  (lambda () (interactive) (als-wrap-previous-object "dot"))
    ".. " (lambda () (interactive) (als-wrap-previous-object "dot"))
    ",."  (lambda () (interactive) (als-wrap-previous-object "vec"))
    ".,"  (lambda () (interactive) (als-wrap-previous-object "vec"))
    "~ "  (lambda () (interactive) (als-wrap-previous-object "tilde"))
    "hat" (lambda () (interactive) (als-wrap-previous-object "hat"))
    "bar" (lambda () (interactive) (als-wrap-previous-object "overline")))
  "Default snippets, for use when defining `als-prefix-map'.")

(defvar als-prefix-map
  (apply #'als-set-snippets (make-sparse-keymap) als-default-snippets)
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
                                           '<)))
                   als-current-prefix-maps)))

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
