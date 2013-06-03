(require 'package)
(package-initialize)

;; (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
			 ("marmalade" . "http://marmalade-repo.org/packages/")
			 ("melpa" . "http://melpa.milkbox.net/packages/")))

(evil-mode 1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(global-linum-mode t)

(set-frame-font "Inconsolata-g-14" nil)
(set-face-attribute 'default nil :font "Inconsolata-g-14")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes (quote ("f41fd682a3cd1e16796068a2ca96e82cfd274e58b978156da0acce4d56f2b0d5" "41b6698b5f9ab241ad6c30aea8c9f53d539e23ad4e3963abff4b57c0f8bf6730" "1affe85e8ae2667fb571fc8331e1e12840746dae5c46112d5abb0c3a973f5f5a" "e53cc4144192bb4e4ed10a3fa3e7442cae4c3d231df8822f6c02f1220a0d259a" "9bac44c2b4dfbb723906b8c491ec06801feb57aa60448d047dbfdbd1a8650897" "d2622a2a2966905a5237b54f35996ca6fda2f79a9253d44793cfe31079e3c92b" "72cc9ae08503b8e977801c6d6ec17043b55313cda34bcf0e6921f2f04cf2da56" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(load-theme 'base16-tomorrow t)
(add-to-list 'auto-mode-alist '("\\.coffee\\'" . coffee-mode))

(define-key evil-normal-state-map ",j" 'evil-window-down)
(define-key evil-normal-state-map ",k" 'evil-window-up)
(define-key evil-normal-state-map ",h" 'evil-window-left)
(define-key evil-normal-state-map ",l" 'evil-window-right)

(define-key evil-normal-state-map ",J" 'evil-window-move-very-bottom
(define-key evil-normal-state-map ",K" 'evil-window-move-very-top)
(define-key evil-normal-state-map ",H" 'evil-window-move-far-left)
(define-key evil-normal-state-map ",L" 'evil-window-move-far-right)
