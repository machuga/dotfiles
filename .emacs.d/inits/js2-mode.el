(add-to-list 'auto-mode-alist '("\\.js$" "\\.json$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(setq-default js2-basic-offset 2)

(eval-after-load "js-mode"
  '(progn
     (setq js2-minor-mode nil)))

(eval-after-load "js2-mode"
  '(progn
     (setq js2-minor-mode nil)))

;(defun activate-js2-paredit-curly ()
;  (define-key js2-mode-map "{" 'paredit-open-curly)
;  (define-key js2-mode-map "}" 'paredit-close-curly))

;(add-hook 'js2-mode-hook 'evil-paredit-mode)
;(add-hook 'js2-mode-hook 'activate-js2-paredit-curly)
