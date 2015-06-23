(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.hbs\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.blade.php\\'" . web-mode))

(add-hook 'web-mode-hook  (lambda ()
  (setq web-mode-markup-indent-offset 2)))

(setq web-mode-engines-alist
      '(("erb"  . "\\.html\\.erb")
        ("erb"  . "\\.erb")
        ("blade"  . "\\.blade\\.php")))
