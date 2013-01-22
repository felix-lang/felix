;; Felix-mode

(require 'generic-x)

(define-generic-mode 
  'felix-mode                     ;; name of the mode
  '("//")                         ;; comments delimiter
  '(
     "union"
     "pod"
     "ctor"
     "const"
     "let"
     "with"
     "match"
     "endmatch"
     "private"
     "include"
     "requires"
     "gen"
     "typedef"
     "class"
     "type"
     "instance"
     "inherit"
     "if"
     "elif"
     "then"
     "else"
     "endif"
     "function"
     "var" 
     "return" 
     "val" 
     "fun"
     "proc"
     "open"
     "for"
     "do"
     "done"
     "goto"
     "not"
     "while"
     "true"
     "false"
     "enum "
     "and"
     "syntax"
     "struct"
   )                              ;; some keywords
  '(("=" . 'font-lock-operator-face) 
    (":" . 'font-lock-operator-face)
    ("$" . 'font-lock-operator-face)
    ("+" . 'font-lock-operator-face)
    ("-" . 'font-lock-operator-face)
    ("<" . 'font-lock-operator-face)
    (">" . 'font-lock-operator-face)
    ("[0-9]+" . 'font-lock-variable-name-face)
    (";" . 'font-lock-builtin-face))   ;; a built-in 
  '("\\.$")                       ;; files that trigger this mode
   nil                            ;; any other functions to call
  "Felix highlighting mode"       ;; doc string
)

(defvar font-lock-operator-face 'font-lock-operator-face)

(defface font-lock-operator-face
  '((((type tty) (class color)) nil)
    (((class color) (background light))
     (:foreground "dark red"))
    (t nil))
  "Used for operators."
  :group 'font-lock-faces)

(add-to-list 'auto-mode-alist '("\\.flx\\'" . felix-mode))
