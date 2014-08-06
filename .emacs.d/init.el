;; Package management
(add-to-list 'load-path "~/.emacs.d")
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")


(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")
                         ("org" . "http://orgmode.org/elpa/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))
(require 'package)
(package-initialize)

(add-to-list 'el-get-recipe-path "~/.emacs.d/recipes")

(setq
 ;; el-get variables
 el-get-verbose t
 el-get-user-package-directory "~/.emacs.d/inits"

 my-packages '(evil
               powerline
               magit
               fiplr
               auto-complete
               flycheck
               web-mode
               ag
               slim-mode
               evil-surround evil-leader evil-numbers
               php-mode-improved php-completion
               shell-switcher
               ruby-mode inf-ruby ruby-end rinari rspec-mode
               highlight-indentation
               ibuffer-vc
               js2-mode js-comint
               coffee-mode
               yaml-mode haml-mode markdown-mode ;; slim-mode
               ;;color-theme-almost-monokai
               scss-mode css-mode))

;; load up some more various configs and modes
(mapc 'require
      '(cl my-defuns uniquify display))

(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)

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
(set-face-attribute 'default nil :font  "Monaco-14")

(let ((path (shell-command-to-string ". ~/.zshenv; echo -n $PATH")))
  (setenv "PATH" path)
  (setq exec-path
        (append
         (split-string-and-unquote path ":")
         exec-path)))


(load "evil-bindings.el")
(load "term-bindings.el")
