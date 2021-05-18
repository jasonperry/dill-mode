;; Syntax highlighting and indentation for the Dill language.

(defvar dill-mode-hook nil)

(add-to-list 'auto-mode-alist '("\\.dl\\'" . dill-mode))
(add-to-list 'auto-mode-alist '("\\.dms\\'" . dill-mode))

;(regexp-opt '("var" "if" "then" "elsif" "else" "endif" "end" "while"
;	      "loop" "endloop" "proc" "return" "module" "modspec"
;	      "import" "as" "open" "export" "private" "type" "struct"
;	      "mut"))

(defconst dill-font-lock-keywords-1
  (list
   '("\\<\\(?:as\\|e\\(?:ls\\(?:e\\|if\\)\\|nd\\(?:if\\|loop\\)?\\|xport\\)\\|i\\(?:f\\|mport\\)\\|loop\\|m\\(?:od\\(?:spec\\|ule\\)\\|ut\\)\\|open\\|pr\\(?:ivate\\|oc\\)\\|return\\|struct\\|t\\(?:hen\\|ype\\)\\|var\\|while\\)\\>" . font-lock-builtin-face))
  "Keyword highlighting expressions for dill-mode")

(defvar dill-font-lock-keywords dill-font-lock-keywords-1
  "Default highlighting expressions for WPDL mode")

;; word following "Type" or a colon is a type, and also the end keyword
(defvar dill-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?\( "()1n" st)
    (modify-syntax-entry ?\) ")(4n" st)
    (modify-syntax-entry ?* ". 23n" st)
    st)
  "Syntax table for dill-mode")

;; indent after line ending in =, struct, then, loop

;; unindent after "end" something

(defun dill-mode ()
  "Major mode for editing Workflow Process Description Language files"
  (interactive)
  (kill-all-local-variables)
  (set-syntax-table dill-mode-syntax-table)
  ;;(use-local-map wpdl-mode-map)
  (set (make-local-variable 'font-lock-defaults) '(dill-font-lock-keywords))
  ;;   (set (make-local-variable 'indent-line-function) 'dill-indent-line)
  (setq major-mode 'dill-mode)
  (setq mode-name "Dill")
  (run-hooks 'dill-mode-hook))

(provide 'dill-mode)
