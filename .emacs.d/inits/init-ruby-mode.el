(add-to-list 'auto-mode-alist
	     '("\\.\\(?:gemspec\\|irbrc\\|gemrc\\|rake\\|rb\\|ru\\|thor\\|cap\\)\\'" . ruby-mode))
(add-to-list 'auto-mode-alist
	     '("\\(Capfile\\|Gemfile\\(?:\\.[a-zA-Z0-9._-]+\\)?\\|[rR]akefile\\)\\'" . ruby-mode))

(defun ruby-toggle-symbol-string ()
  (interactive)
  (save-excursion
    (let (start stop to-replace)
      (setq start (point))
      (setq first-char (buffer-substring-no-properties start (+ 1 start)))
      (ruby-forward-sexp)
      (setq stop (point))
      (setq to-replace (buffer-substring start stop))
      (delete-region start stop)
      (if (string= first-char ":")
	  (insert "\""
		  (replace-regexp-in-string ":" "" to-replace)
		  "\"")
	(insert ":"
		(replace-regexp-in-string "\"" "" to-replace)))
      )))

;;(define-key ruby-mode-map (kbd "C-c :") 'ruby-toggle-symbol-string)
;; Ruby-mode not available yet - lazy loaded?
