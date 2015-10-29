(global-flycheck-mode)
(add-hook 'js2-mode-hook
          (lambda () (flycheck-mode t)))
