(provide 'wal)

(defconst wal-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; ; is a comment starter
    (modify-syntax-entry ?\; "<" table)
    ;; \n is a comment ender
    (modify-syntax-entry ?\n ">" table)    
    table))

(setq wal-op-arithmetic '("+" "-" "*" "/" "fdiv" "**") )
(setq wal-op-logical '(">" "<" "=" "<=" ">=" "&&" "||") )
(setq wal-op-keywords '("print" "printf" "set" "let" "call" "import"
		     "require" "defun" "list" "first" "second"
		     "slice" "array" "rest" "map" "mapa"
		     "eval" "quote" "get" "seta" "geta") )
(setq wal-op-control-flow '("if" "cond" "when" "unless" "while" "for") )
(setq wal-op-special '("whenever" "find" "count" "groups" "in-group" "in-groups"
			    "in-scope" "in-scopes" "resolve-group" "resolve-scope") )


(setq wal-keywords-regexp (regexp-opt wal-op-keywords 'words))
(setq wal-arithmetic-regexp (regexp-opt wal-op-arithmetic 'words))
(setq wal-logical-regexp (regexp-opt wal-op-logical 'words))
(setq wal-control-flow-regexp (regexp-opt wal-op-control-flow 'words))
(setq wal-special-regexp (regexp-opt wal-op-special 'words))

(setq wal-highlights
      `(;; Color function definitions
	("defun \\([/[:alnum:].-]+\\)" 1 'font-lock-function-name-face)
	(,wal-keywords-regexp . font-lock-keyword-face)
	(,wal-arithmetic-regexp . font-lock-keyword-face)
	(,wal-logical-regexp . font-lock-keyword-face)
	(,wal-control-flow-regexp . font-lock-keyword-face)
	(,wal-special-regexp . font-lock-type-face)

	;; Color constants
	("#t\\|#f" . font-lock-constant-face)
	;; Color user function calls
	("\(\s*\\(\\w+\\)\s" 1 'font-lock-builtin-face)
	;; Color variable bindings
	("\\(set\\|let\\|for\\) \\[\\(\\w+\\)\s" 2 'font-lock-variable-name-face)
	;; Color timed evaluation - for now only integers and symbols
	("[@#~][/[:alnum:].-]+" . font-lock-string-face)
	;; Color numbers
	("-?[[:digit:]]+" . font-lock-constant-face)))

(define-derived-mode wal-mode prog-mode "WAL Mode"
  :syntax-table wal-mode-syntax-table
  (setq font-lock-defaults '(wal-highlights))
  (font-lock-fontify-buffer))
