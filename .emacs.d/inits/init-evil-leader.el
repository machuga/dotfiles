(global-evil-leader-mode)

(evil-leader/set-key
  "b" 'ibuffer
  "e" 'eval-buffer
  "f" 'fiplr-find-file
  "s" '(lambda ()
         (interactive)
         (split-window-horizontally)
         (switch-to-buffer "*scratch*")
         (balance-windows))
  "i" 'open-init-file
  "g" 'magit-status
  "p" 'evil-buffer
  "w" 'whitespace-cleanup)
