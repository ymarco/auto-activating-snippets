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

(defvar als-pre-snippet-expand-hook nil
  "Hooks to run just before expanding snippets.")
(defvar als-post-snippet-expand-hook nil
  "Hooks to run just after expanding snippets.")

(defun als-expand-snippet-maybe (key expansion &optional condition)
  "Expand snippet with KEY as EXPANSION.

When CONDITION is a function, call (from the position in the
buffer exactly before the key) and do not expand if it returned
nil. CONDITION is expected not to modify the buffer.

EXPANTION is called interactively, and CONDITION
non-interactively."
  (when (and
         ;; key was fully typed
         (save-excursion
           (search-backward key (- (point) (length key)) t))
         ;; condition is either not present, or evaluates to true
         (or (null condition)
             (backward-char (length key))
             (prog1 (funcall condition)
               (forward-char (length key)))))
    (delete-char (- (length key)))
    (run-hooks 'als-pre-snippet-expand-hook)
    (if (functionp expansion)
        (call-interactively expansion)
      (insert expansion))
    (run-hooks 'als-post-snippet-expand-hook)
    t))


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

(defun als-set-expanding-ligatures (keymap &rest args)
  "Set multiple keys and expansions on KEYMAP for `als-set-expanding-ligature'.

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
          (als-define-prefix-map-snippet keymap key expansion cond))))))

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

     ;; "to"  "\\to"
     :cond #'als-auto-index-condition
     :cond-desc "In math and after a single letter"
     :desc "Automatic subscripts"
     "ii" #'als-insert-subscript
     "jj" #'als-insert-subscript
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

(defvar-local als-current-prefix-maps nil
  "Global variable to keep track of the current user path trace of snippets.

Gets updated by `als-post-self-insert-hook'.")

(defun als-post-self-insert-hook ()
  "TODO."
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
