;;; packages/Auto-LaTeX-Snippets/latex-snippets.el -*- lexical-binding: t; -*-

(defun als-current-snippet-insert-post-space-if-wanted ()
  (when (and (stringp als-transient-snippet-expansion)
             (= ?\\ (aref als-transient-snippet-expansion 0))
             (not (memq (char-after) '(?\) ?\]))))
    (insert " ")))

(add-hook 'LaTeX-mode-hook
          (lambda ()
            (add-hook 'als-post-snippet-expand-hook
                      #'als-current-snippet-insert-post-space-if-wanted
                      nil t)))



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
             collect (let ((expp exp)) (lambda () (interactive)
                                         (als-wrap-previous-object expp)))))
  "A simpler way to apply accents. Expand If LaTeX symbol immidiately before point.")

(defvar als-prefix-map
  (let ((keymap (make-sparse-keymap)))
    (apply #'als-set-snippets keymap als-basic-snippets)
    (apply #'als-set-snippets keymap als-subscript-snippets)
    (apply #'als-set-snippets keymap als-frac-snippet)
    (apply #'als-set-snippets keymap als-accent-snippets)
    keymap)
  "Defalut snippet keymap.")
