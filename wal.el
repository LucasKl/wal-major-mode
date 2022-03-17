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
  (font-lock-fontify-buffer)
  (local-set-key (kbd "M-<tab>") 'wal-complete-symbol))

(add-to-list 'auto-mode-alist '("\\.wal\\'" . wal-mode))

;; Completion
(require 'ido) ; part of emacs

;; this is your lang's keywords
(setq wal-keywords
      '("!" "atom?" "fold" "int?" "range" "symbol?" "!="
	"average" "fold/signal" "lambda" "require" "type"
	"&&" "call" "for" "last" "resolve-group"
	"unalias" "*" "case" "get" "length"
	"resolve-scope" "unless" "**" "cond" "geta"
	"let" "rest" "unload" "+" "convert/bin     geta/default"
	"letret" "reval" "unset-scope" "-" "count"
	"groups" "list" "second" "when" "/"
	"defun" "help" "list?" "set" "whenever"
	"<" "do" "if" "load" "set-scope"
	"while" "<=" "eval" "import" "map"
	"seta" "zip" "=" "exit" "in"
	"mapa" "slice" "||" ">" "fdiv"
	"in-group" "max" "step" ">=" "find"
	"in-groups" "min" "string->int" "alias" "find/g"
	"in-scope" "print" "string?" "all-scopes" "first"
	"inc" "printf" "sum" "array" "fn"
	"int->string" "quote" "symbol->string"))

(defun wal-complete-symbol ()
  "Perform keyword completion on current symbol.
This uses `ido-mode' user interface for completion."
  (interactive)
  (let* (
         ($bds (bounds-of-thing-at-point 'symbol))
         ($p1 (car $bds))
         ($p2 (cdr $bds))
         ($current-sym
          (if  (or (null $p1) (null $p2) (equal $p1 $p2))
              ""
            (buffer-substring-no-properties $p1 $p2)))
         $result-sym)
    (when (not $current-sym) (setq $current-sym ""))
    (setq $result-sym
          (ido-completing-read "" wal-keywords nil nil $current-sym ))
    (delete-region $p1 $p2)
    (insert $result-sym)))
