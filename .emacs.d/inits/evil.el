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

(setq key-chord-two-keys-delay 0.5)
(key-chord-define evil-insert-state-map "jj" 'evil-normal-state)
(key-chord-mode 1)

(defun define-custom-evil-quit ()
  (evil-define-command evil-quit (&optional force)
     "MODIFIED TO ONLY CLOSE WINDOW. Closes the current window, current frame, Emacs.
If the current frame belongs to some client the client connection
is closed."
     :repeat nil
     (interactive "<!>")
     (condition-case nil
         (delete-window)
       (error
        (if (and (boundp 'server-buffer-clients)
                 (fboundp 'server-edit)
                 (fboundp 'server-buffer-done)
                 server-buffer-clients)
            (if force
                (server-buffer-done (current-buffer))
              (server-edit))
          (message "No more windows to remove. Use 'bd' to destroy this buffer")))))
  (evil-ex-define-cmd "q[uit]" 'evil-quit))
(define-custom-evil-quit)
