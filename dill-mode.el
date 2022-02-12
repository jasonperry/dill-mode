;; Syntax highlighting and indentation for the Dill language.

(defvar dill-mode-hook nil)

(add-to-list 'auto-mode-alist '("\\.dl\\'" . dill-mode))
(add-to-list 'auto-mode-alist '("\\.dms\\'" . dill-mode))

;(regexp-opt '("var" "if" "then" "elsif" "else" "endif" "begin" "end" "while"
;	      "loop" "endwhile" "proc" "return" "module" "modspec"
;	      "import" "as" "open" "export" "private" "type" "is" "struct"
;	      "variant" "mut"))

(defconst dill-font-lock-keywords-1
  (list ; the angle brackets aren't given, why did I add them?
   '(
     "\\<\\(?:as\\|begin\\|e\\(?:ls\\(?:e\\|if\\)\\|nd\\(?:if\\|while\\)?\\|xport\\)\\|i\\(?:mport\\|[fs]\\)\\|loop\\|m\\(?:od\\(?:spec\\|ule\\)\\|ut\\)\\|open\\|pr\\(?:ivate\\|oc\\)\\|return\\|struct\\|t\\(?:hen\\|ype\\)\\|var\\(?:iant\\)?\\|while\\)\\>"
     . font-lock-builtin-face))
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

(defun dill-indent-line ()
  (interactive)
  (beginning-of-line)
  (if (bobp)
      (indent-line-to 0)
    (let ((not-indented t) cur-indent)
    ;; unindent a line with "end" something (endif, endwhile, end <ident>)
      (if (looking-at "^[ \t]*\\(endif\\|endwhile\\|end \\)")
	  (progn
            (save-excursion
              (forward-line -1)
              (setq cur-indent (- (current-indentation) tab-width)))
	    (if (< cur-indent 0)
		(setq cur-indent 0)))
      ;; look-behind cases.
	(save-excursion
	  ;; do we really need a loop? Can't we just look one behind and
	  ;; keep it the same? We have to look past blank lines at least
	  (while not-indented
	    (forward-line -1)
	    ;; if previous line has an ending keyword, keep current indentation
	    (if (looking-at "^[ \t]*\\(endif\\|endwhile\\|end \\)")
                (progn
                  (setq cur-indent (current-indentation))
                  (setq not-indented nil))
              ;; After a block-starting line, indent further
	      ;; is the anything glob at start better than looking-back?
	      ;; TODO: don't indent after module begin
              (if (looking-at ".*\\(begin\\|then\\|struct\\|variant\\|loop\\)[ \t]*$")
                  (progn
                    (setq cur-indent (+ (current-indentation) tab-width))
                    (setq not-indented nil))
                (if (bobp) ; Check for rule 5
                    (setq not-indented nil))))))
	)
      (if cur-indent
	  (indent-line-to cur-indent)
	(indent-line-to 0))))) ; really? shouldn't we leave it at current-indent?

      ;; indent after line ending in begin, struct, variant, then, loop
      ;;; (if (looking-at "\\(begin\\|then\\|struct\\|variant\\|loop\\)[ \t]*$")



(defun dill-mode ()
  "Major mode for editing Workflow Process Description Language files"
  (interactive)
  (kill-all-local-variables)
  (set-syntax-table dill-mode-syntax-table)
  ;;(use-local-map wpdl-mode-map)
  (set (make-local-variable 'font-lock-defaults) '(dill-font-lock-keywords))
  ;;   (set (make-local-variable 'indent-line-function) 'dill-indent-line)
  (setq indent-tabs-mode nil)
  (setq tab-width 3)
  (set (make-local-variable 'indent-line-function) 'dill-indent-line)
  (setq major-mode 'dill-mode)
  (setq mode-name "Dill")
  (run-hooks 'dill-mode-hook))

(provide 'dill-mode)
