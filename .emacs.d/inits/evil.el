(evil-mode 1)

(setq evil-default-cursor t
      evil-emacs-state-cursor  '("red" box) ; BE CAREFUL WE ARE IN EMACS MODE!!!!
      evil-insert-state-cursor '("white" bar)
      evil-visual-state-cursor '("white" hollow)
      evil-normal-state-cursor '("white" box))

;; esc quits all the things
(fill-keymaps (list evil-normal-state-map
                    evil-visual-state-map)
              [escape] 'keyboard-quit)
(fill-keymaps (list minibuffer-local-map
                    minibuffer-local-ns-map
                    minibuffer-local-completion-map
                    minibuffer-local-must-match-map)
              [escape] 'minibuffer-keyboard-quit)
