(add-to-list 'auto-mode-alist '("\\.js$" "\\.json$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(setq-default js2-basic-offset 2)

(eval-after-load "js2-mode"
  '(progn
     (setq js2-minor-mode nil)))
