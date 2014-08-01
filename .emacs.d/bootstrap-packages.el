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
	       handlebars-mode
	       web-mode
	       ag
	       slim-mode
	       evil-leader evil-numbers
	       evil-surround
	       php-mode-improved php-completion
	       shell-switcher
	       ruby-mode inf-ruby ruby-end rinari rspec-mode
	       ibuffer-vc
	       js2-mode js-comint
	       coffee-mode
	       ac-coffee
	       yaml-mode haml-mode markdown-mode ;; slim-mode
	       ;color-theme-almost-monokai
	       scss-mode css-mode))

(provide 'bootstrap-packages)
