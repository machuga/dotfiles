(defvar my-font (if (member "Source Code Pro" (font-family-list))
                    "Source Code Pro-15"
                  (if (member "Inconsolata\-g" (font-family-list))
                      "Inconsolata\-g-15"
                    "Monaco-15")))

(custom-set-faces
 '(default ((t (:family my-font
                :foundry nil
                :slant normal
                :weight normal
                :height 150
                :width normal)))))

;;;(set-frame-font my-font nil)

(set-face-attribute 'default nil :font my-font)
(load-theme 'sanityinc-tomorrow-night t)

(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(when (fboundp 'global-hl-line-mode) (global-hl-line-mode t))

(global-linum-mode 1)

(when window-system
  (blink-cursor-mode 0)
  (global-font-lock-mode t)
  (setq default-frame-alist '((cursor-type . (bar . 3))))

  (setq
   indicate-empty-lines t
   ns-pop-up-frames nil
   ns-use-srgb-colorspace t))

(defvar my-linum-format-string "%4d")

(setq
 ;; Silence that damn bell
 bell-volume 0
 ring-bell-function 'ignore

 ;; Show time in modeline
 display-time-mode 1

 inhibit-splash-screen t
 inhibit-startup-message t

 shell-prompt-pattern "^[^a-zA-Z].*[#$%>] *"

 ;; ediff should use the selected frame
 ediff-window-setup-function 'ediff-setup-windows-plain
 ediff-split-window-function 'split-window-horizontally)

;; Show matching parens
(show-paren-mode 1)

;; Shorten prompt so 'yes' isn't needed
(fset 'yes-or-no-p 'y-or-n-p)




;; Line number formatting
(defun my-linum-get-format-string ()
  (let* ((width (length (number-to-string
                         (count-lines (point-min) (point-max)))))
         (format (concat "%" (number-to-string width) "d")))
    (setq my-linum-format-string format)))

(defun my-linum-format (line-number)
  (propertize (format my-linum-format-string line-number) 'face 'linum))

(setq linum-format 'my-linum-format)

(add-hook 'linum-before-numbering-hook 'my-linum-get-format-string)

;; Mark certain keywords with warning faces
(defun enable-warning-keyword-hiliting (modes)
  "Add hilighting of certain keywords to given modes."
  (dolist (mode modes)
    (font-lock-add-keywords mode
                            '(("\\<\\(FIXME\\|WARNING\\|NOTE\\|TODO\\|TBC\\|TBD\\)[: ]" 1
                               font-lock-warning-face t)))))

(provide 'display)
