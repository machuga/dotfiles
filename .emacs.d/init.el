;; Package management
(add-to-list 'load-path "~/.emacs.d")
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(require 'bootstrap-packages)
(require 'cl)
(require 'my-defuns)
(require 'uniquify)
(require 'display)

(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
(add-hook 'before-save-hook 'whitespace-cleanup)

;; UTF-8 Encoding
(prefer-coding-system       'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

(setq
 ;; Ctags
 path-to-ctags "/usr/local/bin/ctags"

 ;; backups
 backup-directory-alist `((".*" . ,temporary-file-directory))
 auto-save-file-name-transforms `((".*" ,temporary-file-directory))

 ns-pop-up-frames nil

 ;; uniquify
 uniquify-buffer-name-style 'post-forward
 uniquify-separator ":"

 mac-option-modifier 'meta
 ;;mac-command-modifier 'meta

 ;; Silence that damn bell
 bell-volume 0
 ring-bell-function 'ignore
 
 ;; Show time in modeline
 display-time-mode 1

 ;; Don't use lockfiles
 create-lockfiles nil
 ;; tramp
 tramp-default-method "ssh"

 inhibit-splash-screen t
 inhibit-startup-message t

 shell-prompt-pattern "^[^a-zA-Z].*[#$%>] *"

 comint-buffer-maximum-size 10240

 ;; ediff should use the selected frame
 ediff-window-setup-function 'ediff-setup-windows-plain
 ediff-split-window-function 'split-window-horizontally)

(el-get 'sync my-packages)


;; tabs
(setq-default
 indent-tabs-mode nil
 truncate-lines t
 tab-width 2
 coffee-tab-width 2)

(show-paren-mode 1)
(fset 'yes-or-no-p 'y-or-n-p)

(require 'ido)
(ido-mode t)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((ruby . t)
   (sh . t)
   (emacs-lisp . t)))

;; some global keybindings
(global-set-key (kbd "M-ESC") 'jump-to-end)
(global-set-key (kbd "<f1>") 'magit-status)

;;(set-frame-font "Inconsolata-g-15" nil)
(set-face-attribute 'default nil :font "Monaco-14")

(let ((path (shell-command-to-string ". ~/.zshenv; echo -n $PATH")))
  (setenv "PATH" path)
  (setq exec-path
        (append
         (split-string-and-unquote path ":")
         exec-path)))

(load "term-bindings.el")
