;; Syntax highlighting and indentation for the Dill language.

(defvar dill-mode-hook nil)

(add-to-list 'auto-mode-alist '("\\.dl\\'" . dill-mode))
(add-to-list 'auto-mode-alist '("\\.dms\\'" . dill-mode))

;; For the developer, to generate the regex string ahead-of-time
;(regexp-opt '("var" "if" "then" "elsif" "else" "endif" "begin" "end" "while"
;	       "loop" "endwhile" "case" "of" "endcase" "proc" "return"
;              "module" "modspec" "import" "as" "open" "export"
;              "private" "type" "is" "struct" "variant" "mut")
; 'symbols) ; 'symbols makes the angle brackets

(defconst dill-font-lock-keywords-1
  (list
   '(
     "\\_<\\(as\\|begin\\|case\\|e\\(?:ls\\(?:e\\|if\\)\\|nd\\(?:case\\|if\\|while\\)?\\|xport\\)\\|i\\(?:mport\\|[fs]\\)\\|loop\\|m\\(?:od\\(?:spec\\|ule\\)\\|ut\\)\\|o\\(?:f\\|pen\\)\\|pr\\(?:ivate\\|oc\\)\\|return\\|struct\\|t\\(?:hen\\|ype\\)\\|var\\(?:iant\\)?\\|while\\)\\_>"
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

;; TODO: handle comments (ultimately need to track depth *)
(defun dill-indent-line ()
  (interactive)
  (beginning-of-line)
  ;; easy cases for things that can only appear at the top level.
  (if (or (bobp)
	  (looking-at "^[ \t]*\\(module\\|import\\|open\\|type\\|proc\\)"))
      (indent-line-to 0)
    (let ((not-indented t)
	  cur-indent)
      ;; unindent a line with "end" something - need special case for case--of
      (if (looking-at "^[ \t]*\\(else\\|elsif\\|endif\\|endwhile\\|endcase\\|end \\)")
	  (progn
            (save-excursion
              (forward-line -1)
              (setq cur-indent (- (current-indentation) tab-width)))
	    (if (< cur-indent 0)
		(setq cur-indent 0)))
	;; look-behind cases.
	(save-excursion
	  (while not-indented
	    (forward-line -1)
	    ;; if previous line has an ending keyword, keep current indentation
	    (if (looking-at "^[ \t]*\\(endif\\|endwhile\\|endcase\\|end \\)")
                (progn
                  (setq cur-indent (current-indentation))
                  (setq not-indented nil))
              ;; After a block-starting line, indent further
	      ;; is the anything glob at start better than looking-back?
	      ;; note that 'case' lines have no ending keyword.
	      ;; This won't work if a comment is after. Could quick-fix that...
              (if (looking-at
		   "\\([ \t]*case \\|.*\\(begin\\|then\\|else\\|elsif\\|struct\\|variant\\|loop\\)[ \t]*$\\)")
                  (progn
                    (setq cur-indent (+ (current-indentation) tab-width))
                    (setq not-indented nil))
		;; anything else not a blank line (or in a comment?), use its indent
		(if (not (looking-at "^[ \t]*$")) ; \\|[ \t]*(\\*\\)"))
		    (progn
		      (setq cur-indent (current-indentation))
                      (setq not-indented nil))
                  (if (bobp) ; oops, went back to the beginning 
                      (setq not-indented nil))))))))
      (if cur-indent
	  (indent-line-to cur-indent)
	(indent-line-to 0))))) ; really? shouldn't we leave it at current-indent?


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
