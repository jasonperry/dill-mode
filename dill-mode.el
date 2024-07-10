;; Syntax highlighting and indentation for the Dill language.

(defvar dill-mode-hook nil)

(add-to-list 'auto-mode-alist '("\\.dl\\'" . dill-mode))
(add-to-list 'auto-mode-alist '("\\.dms\\'" . dill-mode))

;; For the developer, to generate the regex string ahead-of-time
;;;(regexp-opt '("var" "if" "then" "elsif" "else" "/if" "while"
;;;    	         "/while" "case" "of" "/case" "proc" "/proc" "return"
;;;              "module" "/module" "modspec" "/modspec" "do" "is"
;;;              "import" "as" "open" "private" "type" "record" "variant"
;;;              "mut")
;;; 'symbols) ; 'symbols makes the angle brackets

(defconst dill-font-lock-keywords-1
  (list
   '(
     "\\_<\\(/\\(?:case\\|if\\|mod\\(?:spec\\|ule\\)\\|proc\\|while\\)\\|as\\|case\\|do\\|els\\(?:e\\|if\\)\\|i\\(?:mport\\|[fs]\\)\\|m\\(?:od\\(?:spec\\|ule\\)\\|ut\\)\\|o\\(?:f\\|pen\\)\\|pr\\(?:ivate\\|oc\\)\\|re\\(?:cord\\|turn\\)\\|t\\(?:hen\\|ype\\)\\|var\\(?:iant\\)?\\|while\\)\\_>"
     . font-lock-keyword-face))
  "Keyword highlighting expressions for Dill")


;; will this work in the middle of a word?
(defconst dill-font-lock-keywords-2
  (append dill-font-lock-keywords-1
	  (list 
	   '("\\<\\([A-Z][a-zA-Z0-9_]*\\)" . font-lock-builtin-face)))
  "Highlighting for keywords and type names"
  )

;; trying to do only types but it's not working
(defvar dill-font-lock-keywords dill-font-lock-keywords-2
  "Highlighting expression for dill-mode")


;; word following "Type" or a colon is a type, and also the end keyword
(defvar dill-mode-syntax-table
  (let ((st (make-syntax-table)))
;;;    (modify-syntax-entry ?\( "()1n" st)
;;;    (modify-syntax-entry ?\) ")(4n" st)
;;;    (modify-syntax-entry ?* ". 23n" st)
    (modify-syntax-entry ?\( "()1nb" st) ; nested is type b
    (modify-syntax-entry ?\) ")(4nb" st)
    (modify-syntax-entry ?*  "_ 123" st) ; doesn't need the b?
    (modify-syntax-entry ?\n ">" st)
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
      ;; 'case' is a special case, because the top line doesn't end with a
      ;; keyword, and the 'of' blocks are indented relative to the 'case'
      ;; (unlike elsif/else blocks).
      ;; could I just punt and have the "of" blocks not be indented?
      ;; that's actually an OK idea, it's the same as function begins.
      ;; ...and it avoids special handling with 'endcase' also.
      ;; oh no, wait. the first 'of' should be different...unless
      ;; ...unless i indent because of "case", then unindent! Yeah!
      ;; ...but the code structure doesn't handle multiple indents.
      ;; conclusion: now 'of' is not indented relative to case, and
      ;;   there is a quick check if the previous line is 'case'.
      
      ;; unindent a line that ends /or ends and starts/ a block.
      (if (looking-at "^[ \t]*\\(else\\|elsif\\|of \\|/if\\|/while\\|/case\\|/proc\\|/module\\|/modspec\\)")
	  (progn
            (save-excursion
              (forward-line -1)
	      ;; special case: don't unindent first "of" inside a case block.
	      (if (looking-at "^[ \t]*case")
		  (setq cur-indent (current-indentation))
		(setq cur-indent (- (current-indentation) tab-width))))
	    (if (< cur-indent 0)
		(setq cur-indent 0)))
	;; look-behind cases.
	(save-excursion
	  (while not-indented
	    (forward-line -1)
	    ;; if previous line has an ending keyword, keep current indentation
	    ;; ...do I even need this if that's the default?
	    (if (looking-at "^[ \t]*\\(/module\\|/modspec\\|/proc\\|/if\\|/while\\|/case\\|/type\\)")
                (progn
                  (setq cur-indent (current-indentation))
                  (setq not-indented nil))
              ;; After a block-starting line, indent further
	      ;; is the .* at start of line better than looking-back?
	      ;; This won't work if a comment is after. Could quick-fix that...
              (if (looking-at
		   ".*\\(do\\|then\\|else\\|elsif\\|record\\|variant\\)[ \t]*$")
		  ;; TODO: don't indent if it's a module begin
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
  "Major mode for editing Dill programming language source files"
  (interactive)
  (kill-all-local-variables)
  (set-syntax-table dill-mode-syntax-table)
  ;;(use-local-map wpdl-mode-map)
  (set (make-local-variable 'font-lock-defaults) '(dill-font-lock-keywords))
  ;;;(set (make-local-variable 'font-lock-defaults) '(dill-font-locks))
  (setq indent-tabs-mode nil)
  (setq tab-width 3)
  (set (make-local-variable 'indent-line-function) 'dill-indent-line)
  (setq major-mode 'dill-mode)
  (setq mode-name "Dill")
  (run-hooks 'dill-mode-hook))

(provide 'dill-mode)
