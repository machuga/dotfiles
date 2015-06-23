(add-to-list 'load-path "~/.emacs.d/lisp")

(require 'cl)
(require 'tweaks)
(require 'package-load)
(require 'terminal)
(require 'display)

;; UTF-8 Encoding
(prefer-coding-system       'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
(add-hook 'before-save-hook 'whitespace-cleanup)

(setq
 default-directory "~/"

 ;; backups
 backup-directory-alist `((".*" . ,temporary-file-directory))
 auto-save-file-name-transforms `((".*" ,temporary-file-directory))

 ;; Ctags
 path-to-ctags "/usr/local/bin/ctags"

 ;; Modifier keys
 mac-option-modifier 'meta
 ;;mac-command-modifier 'meta

 ;; Don't use lockfiles
 create-lockfiles nil

 ;; tramp
 tramp-default-method "ssh"

 comint-buffer-maximum-size 10240

 ;; Always add newline
 require-final-newline t

 ;; Don't make backups
 make-backup-files nil)

;; tabs
(setq-default
 indent-tabs-mode nil
 truncate-lines t
 tab-width 2
 coffee-tab-width 2)

(let ((path (shell-command-to-string ". ~/.zshenv; echo -n $PATH")))
  (setenv "PATH" path)
  (setq exec-path
  (append
   (split-string-and-unquote path ":")
   exec-path)))

(require 'smooth-scrolling)
(setq smooth-scroll-margin 5)
(setq scroll-conservatively 9999
      scroll-preserve-screen-position t)

;;(custom-set-variables
;; '(initial-frame-alist '(fullscreen . maximized)))
