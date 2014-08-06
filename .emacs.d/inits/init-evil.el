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

(define-key evil-normal-state-map ",j" 'evil-window-down)
(define-key evil-normal-state-map ",k" 'evil-window-up)
(define-key evil-normal-state-map ",h" 'evil-window-left)
(define-key evil-normal-state-map ",l" 'evil-window-right)

(define-key evil-normal-state-map ",J" 'evil-window-move-very-bottom)
(define-key evil-normal-state-map ",K" 'evil-window-move-very-top)
(define-key evil-normal-state-map ",H" 'evil-window-move-far-left)
(define-key evil-normal-state-map ",L" 'evil-window-move-far-right)
