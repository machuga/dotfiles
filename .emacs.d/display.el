(custom-set-faces
 '(default ((t (:family "Inconsolata" :foundry nil :slant normal :weight normal :height 150 :width normal)))))

;(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

(when window-system ;; Graphics mode

  (global-font-lock-mode t)
  (setq default-frame-alist '((cursor-type . (bar . 3))))

            ; always fullscreen

  ;(setq ns-use-native-fullscreen nil)
  ;(set-frame-parameter nil 'fullscreen 'fullboth)
  ;;(set-frame-parameter (selected-frame) 'alpha '(90 50))
  ;;(add-to-list 'default-frame-alist '(alpha 80 70))

  (global-linum-mode 1)
  (blink-cursor-mode 0)
  (when (fboundp 'global-hl-line-mode) (global-hl-line-mode t))

  (setq default-indicate-empty-lines t)
  (setq indicate-empty-lines t))


(defvar my-linum-format-string "%4d")
(add-hook 'linum-before-numbering-hook 'my-linum-get-format-string)
(defun my-linum-get-format-string ()
  (let* ((width (length (number-to-string
                         (count-lines (point-min) (point-max)))))
         (format (concat "%" (number-to-string width) "d")))
    (setq my-linum-format-string format)))
(setq linum-format 'my-linum-format)
(defun my-linum-format (line-number)
  (propertize (format my-linum-format-string line-number) 'face 'linum))


;; Mark certain keywords with warning faces
(defun enable-warning-keyword-hiliting (modes)
  "Add hilighting of certain keywords to given modes."
  (dolist (mode modes)
    (font-lock-add-keywords mode
                            '(("\\<\\(FIXME\\|WARNING\\|NOTE\\|TODO\\|TBC\\|TBD\\)[: ]" 1
                               font-lock-warning-face t))
                            )))

;; Replace that horrible emacs 23.x visible-bell square with a mode-line-based
;; one instead.  Thanks, Miles.

;; (defcustom mode-line-bell-string "!"
;;   "Message displayed in mode-line by `mode-line-bell' function."
;;   :group 'user)
;; 
;; (defcustom mode-line-bell-delay 0.1
;;   "Number of seconds `mode-line-bell' displays its message."
;;   :group 'user)
;; 
;; (defvar mode-line-bell-cached-string nil)
;; (defvar mode-line-bell-propertized-string nil)
;; 
;; (defun mode-line-bell ()
;;   "Briefly display a highlighted message in the mode-line.
;; 
;; The string displayed is the value of `mode-line-bell-string',
;; with a red background; the background highlighting extends to the
;; right margin.  The string is displayed for `mode-line-bell-delay'
;; seconds.
;; 
;; This function is intended to be used as a value of `ring-bell-function'."
;;   (unless (equal mode-line-bell-string mode-line-bell-cached-string)
;;     (setq mode-line-bell-propertized-string
;;           (propertize
;;            (concat
;;             (propertize
;;              ""
;;              'display
;;              `(space :align-to (- right 2 ,(string-width mode-line-bell-string))))
;;             mode-line-bell-string)
;;            'face '(:foreground "red")
;;            ))
;;     (setq mode-line-bell-cached-string mode-line-bell-string))
;;   (message mode-line-bell-propertized-string)
;;   (sit-for mode-line-bell-delay)
;;   (message ""))
;; 
;; (setq ring-bell-function 'mode-line-bell)

(provide 'display)
