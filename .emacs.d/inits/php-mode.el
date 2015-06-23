(add-hook 'php-mode-hook 'php-enable-psr2-coding-style)
(add-hook 'php-mode-hook (lambda () (php-template-compatibility nil)))
