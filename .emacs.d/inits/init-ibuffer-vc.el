(global-set-key (kbd "C-x C-b") 'ibuffer)
(autoload 'ibuffer "ibuffer" "List buffers." t)
(add-hook 'ibuffer-mode-hook
          (lambda ()
            (ibuffer-switch-to-saved-filter-groups "default")))

(setq
 ibuffer-never-show-predicates '("\\*Completions\\*"
                                 "\\*Tramp*"
                                 "\\*Compile-log*"
                                 "\\*Ediff*"
                                 "\\*buffer-selection\\*"
                                 "\\*Buffer List\\*"
                                 "\\*Help\\*"
                                 "\\*Packages\\*")

 ibuffer-saved-filter-groups '(("default"
                                ("rb"
                                 (mode . ruby-mode)
                                 (mode . inf-ruby))
                                ("js" (or
                                       (mode . js2-mode)
                                       (mode . javascript-mode)))
                                ("php" (mode . php-mode))
                                ("elisp" (mode . emacs-lisp-mode))
                                ("shell" (or
                                          (name . "^\\*eshell")
                                          (name . "^\\*ansi-term*")))
                                 
                                ("magit" (name . "^\\*magit"))
                                ("emacs" (or
                                          (name . "^\\*scratch\\*$")
                                          (name . "^\\*Messages\\*$"))))))
