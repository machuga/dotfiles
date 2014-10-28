(evil-mode 1)

(setq evil-default-cursor t
      evil-emacs-state-cursor  '("red" box) ; BE CAREFUL WE ARE IN EMACS MODE!!!!
      evil-operator-state-cursor '("red" hollow)
      evil-replace-state-cursor '("red" bar)
      evil-insert-state-cursor '("white" bar)
      evil-visual-state-cursor '("orange" box)
      evil-normal-state-cursor '("green" box))

;; esc quits all the things
(fill-keymaps (list evil-normal-state-map
                    evil-visual-state-map)
              [escape] 'keyboard-quit)
(fill-keymaps (list minibuffer-local-map
                    minibuffer-local-ns-map
                    minibuffer-local-completion-map
                    minibuffer-local-must-match-map)
              [escape] 'minibuffer-keyboard-quit)

(define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
(define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)
