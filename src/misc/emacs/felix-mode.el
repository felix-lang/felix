;; Felix-mode

(require 'generic-x)

(define-generic-mode 
  'felix-mode                     ;; name of the mode
  '("//")                         ;; comments delimiter
  '(
    "chip" "pin" "read" "write"
    "circuit" "endcircuit" "connector" "connect" "device"
    "axiom" "lemma" "in"
    "object" "implements" "interface"
    "export"
    "perform"
    "method"
    "array" "darray" "varray" "opt" "int" "uint" "string"
     "union"
     "pod"
     "ctor"
     "const"
     "let"
     "with"
     "match"
     "typematch"
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
     "bool"
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
  '("\\.flx$")                       ;; files that trigger this mode
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

;; --------------------------------------------------
;; custom views for symbols require Emacs 24.4
;; (should be safe for older versions, just inactive)

(defun pretty-felix-symbols () 
    (setq prettify-symbols-alist
          '(
            ;; automatic
            ("=>" . "⇒")
            ("<-" . "←")
            ("->" . "→")
            ("|>" . "▷")
            ("|-" . "⊢")
            ("\vdash" . "⊢")
            
            ;; basics symbols
            ("\\circ" . "ˆ")
            ("\\vert" . "|")
            ("\\brvbar" . "¦")
            ("\\S" . "§")
            ("\\sect" . "§")
            ("\\amp" . "&")
            ("\\lt" . "<")
            ("\\gt" . ">")
            ("\\tilde" . "~")
            ("\\slash" . "/")
            ("\\plus" . "+")
            ("\\under" . "_")
            ("\\equal" . "=")
            ("\\dagger" . "†")
            ("\\dag" . "†")
            ("\\Dagger" . "‡")
            ("\\ddag" . "‡")


            ;; greek characters
            ("\\Alpha" . "Α")
            ("\\alpha" . "α")
            ("\\Beta" . "Β")
            ("\\beta" . "β")
            ("\\Gamma" . "Γ")
            ("\\gamma" . "γ")
            ("\\Delta" . "Δ")
            ("\\delta" . "δ")
            ("\\Epsilon" . "Ε")
            ("\\epsilon" . "ε")
            ("\\Zeta" ."Ζ")
            ("\\zeta" . "ζ")
            ("\\Eta" . "Η")
            ("\\eta" . "η")
            ("\\Theta" . "Θ")
            ("\\theta" . "θ")
            ("\\thetasym" . "ϑ")
            ("\\vartheta" . "ϑ")
            ("\\Iota" . "Ι")
            ("\\iota" . "ι")
            ("\\Kappa" . "Κ")
            ("\\kappa" . "κ")
            ("\\Lambda" . "Λ")
            ("\\lambda" . "λ")
            ("\\Mu" . "Μ")
            ("\\mu" . "μ")
            ("\\nu" . "ν")
            ("\\Nu" . "Ν")
            ("\\Xi" . "Ξ")
            ("\\xi" . "ξ")
            ("\\Omicron" ."Ο")
            ("\\omicron" . "ο")
            ("\\Pi" . "Π")
            ("\\pi" . "π")
            ("\\Rho" . "Ρ")
            ("\\rho" . "ρ")
            ("\\Sigma" . "Σ")
            ("\\sigma" . "σ")
            ("\\sigmaf" . "ς")
            ("\\Tau" . "Τ")
            ("\\Upsilon" . "Υ")
            ("\\upsilon" . "υ")
            ("\\Phi" . "Φ")
            ("\\phi" . "φ")
            ("\\varphi" . "ɸ")
            ("\\Chi" . "Χ")
            ("\\chi" . "χ")
            ("\\acutex" . "𝑥́")
            ("\\Psi" . "Ψ")
            ("\\psi" . "ψ")
            ("\\tau" . "τ")
            ("\\Omega" . "Ω")
            ("\\omega" . "ω")
            ("\\piv" . "ϖ")
            ("\\varpi" . "ϖ")
            ("\\partial" . "∂")

            ;; Hebrew characters
            ("\\alefsym" . "ℵ")
            ("\\aleph" ."ℵ")
            ("\\gimel" .  "ℷ")
            ("\\beth" . "ב")
            ("\\dalet" . "ד")

            ;; latin
            ("\\fnof" . "ƒ")
            ("\\real" . "ℜ")
            ("\\image" . "ℑ")
            ("\\weierp" . "℘")
            ("\\ell" . "ℓ")
            ("\\imath" . "ı")
            ("\\jmath" . "ȷ")

            ;; Punctuation
            ("\\dots" . "…")
            ("\\cdots" . "⋯")
            ("\\middot" . "·")
            ("\\iexcl" . "¡")
            ("\\iquest" . "¿")
            ("\\laquo" . "«")
            ("\\raquo" . "»")

            ;; science
            ("\\minus" . "−")
            ("\\pm" . "±")
            ("\\plusmn" . "±")
            ("\\times" . "×")
            ("\\over" . "⁄")
            ("\\colon" . ":")
            ("\\div" . "÷")
            ("\\frac12" . "½")
            ("\\frac14" . "¼")
            ("\\frac34" . "¾")
            ("\\permil" . "‰")
            ("\\sup1" . "¹")
            ("\\sup2" . "²")
            ("\\sup3" . "³")
            ("\\radic" . "√")
            ("\\sum" . "∑")
            ("\\prod" . "∏")
            ("\\micro" . "µ")
            ("\\macr" . "¯")
            ("\\deg" . "°")
            ("\\prime" . "′")
            ("\\'" . "′")
            ("\\Prime" . "″")
            ("\\infin" . "∞")
            ("\\infty" . "∞")
            ("\\prop" . "∝")
            ("\\propto" . "∝")
            ("\\not" . "¬")
            ("\\neg" . "¬")
            ("\\land" . "∧")
            ("\\wedge" . "∧")
            ("\\lor" . "∨")
            ("\\vee" . "∨")
            ("\\cap" . "∩")
            ("\\cup" . "∪")
            ("\\int" . "∫")
            ("\\therefore" . "∴")
            ("\\there4" . "∴")
            ("\\because" . "∵")
            ("\\sim" . "∼")
            ("\\cong" . "≅")
            ("\\simeq" . "≅")
            ("\\asymp" . "≈")
            ("\\approx" . "≈")
            ("\\ne" . "≠")
            ("\\neq" "≠")
            ("\\equiv" . "≡")
      
            ("\\triangleq" . "≜")
            ("\\le" . "≤")
            ("\\leq" . "≤")
            ("\\ge" . "≥")
            ("\\geq" . "≥")
            ("\\lessgtr" . "≶")
            ("\\lesseqgtr" . "⋚")
            ("\\ll" . "≪")
            ("\\Ll" . "⋘")
            ("\\lll" . "⋘")
            ("\\gg" . "≫")
            ("\\Gg" "⋙")
            ("\\ggg" . "⋙")
            ("\\prec" . "≺")
            ("\\preceq" . "≼")
            ("\\preccurlyeq" . "≼")
            ("\\succ" . "≻")
            ("\\succeq" . "≽")
            ("\\succcurlyeq" . "≽")
            ("\\sub" . "⊂")
            ("\\subset" . "⊂")
            ("\\sup" . "⊃")
            ("\\supset" . "⊃")
            ("\\nsub" . "⊄")
            ("\\sube" . "⊆")
            ("\\nsup" . "⊅")
            ("\\supe" . "⊇")
            ("\\setminus" . "⧵")
            ("\\forall" . "∀")
            ("\\exist" . "∃")
            ("\\exists" . "∃")
            ("\\nexist" . "∄")
            ("\\nexists" . "∄")
            ("\\empty" . "∅")
            ("\\emptyset" . "∅")
            ("\\isin" . "∈")
            ("\\in" . "∈")
            ("\\notin" . "∉")
            ("\\ni" . "∋")
            ("\\nabla" . "∇")
            ("\\ang" . "∠")
            ("\\angle" . "∠")
            ("\\perp" . "⊥")
            ("\\sdot" . "⋅")
            ("\\cdot" . "⋅")
            ("\\lceil" . "⌈")
            ("\\rceil" . "⌉")
            ("\\lfloor" . "⌊")
            ("\\rfloor" . "⌋")
            ("\\lang" . "⟨")
            ("\\rang" . "⟩")
            ("\\hbar" . "ℏ")
            ("\\mho" . "℧")
      
            ;; Arrows
            ("\\larr" . "←")
            ("\\leftarrow" . "←")
            ("\\gets" . "←")
            ("\\lArr" . "⇐")
            ("\\Leftarrow" . "⇐")
            ("\\uarr" . "↑")
            ("\\uparrow" . "↑")
            ("\\uArr" . "⇑")
            ("\\Uparrow" . "⇑")
            ("\\rarr" . "→")
            ("\\to" . "→")
            ("\\rightarrow" . "→")
            ("\\rArr" . "⇒")
            ("\\Rightarrow" . "⇒")
            ("\\darr" . "↓")
            ("\\downarrow" . "↓")
            ("\\dArr" . "⇓")
            ("\\Downarrow" . "⇓")
            ("\\harr" . "↔")
            ("\\leftrightarrow" . "↔")
            ("\\hArr" . "⇔")
            ("\\Leftrightarrow" . "⇔")
            ("\\crarr" . "↵")
            ("\\hookleftarrow" . "↵")

            ("\\bull" . "•")
            ("\\bullet" . "•")
            ("\\star" . "⋆")
            ("\\lowast" . "∗")
            ("\\ast" . "*")
            ("\\odot" . "ʘ")
            ("\\oplus" . "⊕")
            ("\\otimes" . "⊗")
            ("\\check" . "✓")
            ("\\checkmark" . "✓")
  )))

; bind pretty mode
(add-hook 'felix-mode-hook 'pretty-felix-symbols)

; only enable pretty symbols if emacs can support it.
(if (fboundp 'global-prettify-symbols-mode)
    (global-prettify-symbols-mode 1))


(defun felix-load-file ()
  "Insert the full path file name into the current buffer."
  (interactive)
  (let ((file-name (buffer-file-name
                        (window-buffer
                        (minibuffer-selected-window)))))

    ;;(comint-check-source file-name) ; Check to see if buffer needs
                                    ; saved first

    (message                        ; send output to a new buffer
     (shell-command-to-string       ; capture output
      (concat "flx " file-name))))) ; build command

; bind keyboard shortcuts 
(add-hook 'felix-mode-hook
          ; standard loading of current file
          (lambda () (progn
                  (modify-syntax-entry ?\[ "-")
                  (modify-syntax-entry ?\] "-")
	    (modify-syntax-entry ?\` "-")
                  (local-set-key (kbd "C-c C-l") #'felix-load-file))))



;; Flymake for Felix
(require 'flymake)

;; I don't like the default colors :)
;;(set-face-background 'flymake-errline "yellow")
;;(set-face-background 'flymake-warnline "dark slate blue")


;; Invoke felix with '-c --nocc' to get syntax checking
(defun flymake-felix-init ()
  (let* ((temp-file   (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
	 (local-file  (file-relative-name
                       temp-file
                       (file-name-directory buffer-file-name))))
    (list "flx" (list "-c" "--nocc" local-file))))

(push '(".+\\.flx$"  flymake-felix-init) flymake-allowed-file-name-masks)
(push '(".+\\.fdoc$" flymake-felix-init) flymake-allowed-file-name-masks)

(add-to-list 'flymake-err-line-patterns '("\\(.*\\.flx\\): line \\([0-9]+\\).*$" 1 2 nil))
(add-to-list 'flymake-err-line-patterns '("See also \\(.*\\.flx\\): line \\([0-9]+\\).*$" 1 2 nil))
(add-to-list 'flymake-err-line-patterns '("See: \\(.*\\.flx\\): line \\([0-9]+\\).*$" 1 2 nil))
(add-to-list 'flymake-err-line-patterns '("In \\(.*\\.flx\\): line \\([0-9]+\\).*$" 1 2 nil))
(add-to-list 'flymake-err-line-patterns '("Fatal error.*$" nil))


(add-hook 'felix-mode-hook 'flymake-mode)

