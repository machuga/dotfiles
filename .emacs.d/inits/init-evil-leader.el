(global-evil-leader-mode)


(evil-leader/set-key
  "b" 'ibuffer
  "e" 'eval-buffer
  "g" 'magit-status
  "f" 'fiplr-find-file
  "s" '(lambda ()
         (interactive)
         (split-window-horizontally)
         (switch-to-buffer "*scratch*")
         (balance-windows))
  "w" 'whitespace-cleanup)
