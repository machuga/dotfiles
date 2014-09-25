(require 'package)

(package-initialize)

(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
       ("marmalade" . "http://marmalade-repo.org/packages/")
       ("melpa" . "http://melpa.milkbox.net/packages/")))

(setq url-http-attempt-keepalives nil)

(add-to-list 'load-path "~/.emacs.d/github-packages/")

;; ac-coffee
;; color-theme-almost-monokai

(defvar my-packages
  '(evil magit flycheck
         evil-leader evil-numbers
         evil-surround
         ruby-mode inf-ruby ruby-end rinari rspec-mode
         yaml-mode haml-mode markdown-mode handlebars-mode
         helm
         ibuffer-vc
         js2-mode
         auto-complete
         ac-js2
         highlight-indentation
         coffee-mode
         fiplr
         css-mode scss-mode
         smartparens
         ;; color-theme-almost-monokai
         ag))

(defvar my-github-packages
  '("machuga/powerline" "akfish/ac-coffee")
  "A list of packages to ensure are installed at launch.")

(defun refresh-packages ()
  "Refresh package db with notice"
  (interactive)
  (message "%s" "Refreshing package database...")
  (package-refresh-contents)
  (message "%s" " done."))

(defun my-uninstalled-packages ()
  (remove-if #'package-installed-p my-packages))

;; Need to add fallback for github eventually
(defun install-packages (packages)
  "Custom package installation function. Refreshes contents prior to install if needed."
  (when packages
    (refresh-packages)
    (dolist (p packages) (package-install p))))

(defun init-packages (packages)
  (dolist (p packages)
    (let ((file (format "~/.emacs.d/inits/%s.el" (symbol-name p))))
      (when (file-exists-p file)
        (load-file file)))))

;; Load manual packages
(require 'ido)
(ido-mode t)

;; GitHub package support

(defvar github-package-directory
  "~/.emacs.d/github-packages/"
  "Directory for storing packages from GitHub")

(defun create-github-package-directory ()
  "Create the directory for storing packages from GitHub if it does not exist"
  (interactive)
  (unless (file-exists-p github-package-directory)
    (message "%s" "Creating directory for GitHub packages")
    (make-directory dir)
    (message "%s" "Created directory for GitHub packages")))

(defun install-package-from-github (package)
  "Clone package from GitHub"
  (interactive)
  (let ((url (concat "https://github.com/" package))
        (package-name (cadr (split-string package "/")))
        (package-path (concat github-package-directory (cadr (split-string package "/")))))
    (message "Installing %s package from GitHub" package)
    (if (file-exists-p package-path)
        (progn
          (message "Updating package %s" package))
          ;(shell-command (concat "git -C " package-path " pull")))
        (progn
            (message "Installing package %s" package)
            (clone-package-from-github package package-path)
            (message "Package %s installed" package)))))

(defun install-github-packages ()
  (dolist (p my-github-packages)
    (install-package-from-github p)))


(defun clone-package-from-github (package path)
  "Clone package from GitHub"
  (let ((url (concat "https://github.com/" package)))
    (message "Cloning %s package from GitHub to %s..." package path)
    (if (shell-command
         (concat "git --no-pager clone " url " " path)) ;; This isn't really checking properly - need to tweak
        (message "Cloned successfully!")
      (message "Clone failed :("))))


(defun github-package-initialize ()
  (dolist (p my-github-packages)
    (let ((name (cadr (split-string p "/")))
          (path (concat github-package-directory (cadr (split-string p "/")))))
      (when (file-exists-p path)
        (require '(intern name) path)))))


;; Install required packages
(install-packages (my-uninstalled-packages))

;; Init my packages
(init-packages my-packages)

;; Install required packages
(install-github-packages)

;; Init my GitHub packages
;;(github-package-initialize)
(dolist (p my-github-packages)
  (let ((name (cadr (split-string p "/")))
        (file (format "~/.emacs.d/inits/%s.el" (cadr (split-string (to-string p) "/")))))
    (when (file-exists-p file)
      (add-to-list 'load-path (concat "~/.emacs.d/github-packages/" name))
      (require (intern name))
      (load-file file))))

;;(init-packages my-github-packages)
;;(define-key global-map (kbd "RET") 'newline-and-indent)
(electric-indent-mode 1)
(provide 'package-load)
