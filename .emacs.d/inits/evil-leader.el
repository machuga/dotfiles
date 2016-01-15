(global-evil-leader-mode)

(evil-leader/set-leader "<SPC>")
(evil-leader/set-key
  "j" 'evil-window-down
  "k" 'evil-window-up
  "h" 'evil-window-left
  "l" 'evil-window-right
  "J" 'evil-window-move-very-bottom
  "K" 'evil-window-move-very-top
  "H" 'evil-window-move-far-left
  "L" 'evil-window-move-far-right
  "n" 'neotree-toggle
  "b" 'ibuffer
  "e" 'eval-buffer
  ;;"f" 'fiplr-find-file
  "f" 'helm-projectile-find-file
  "F" '(lambda ()
         (interactive)
         (fiplr-clear-cache)
         (fiplr-find-file))
  "s" 'helm-projectile-ag
  "S" '(lambda ()
         (interactive)
         (split-window-horizontally)
         (switch-to-buffer "*scratch*")
         (balance-windows))
  "i" 'open-init-file
  "g" 'magit-status
  "p" 'evil-buffer
  "w" '(lambda ()
         (interactive)
         (if (= (length (window-list)) 1)
           (winner-undo)
           (delete-other-windows)))
  "ci" 'evilnc-comment-or-uncomment-lines
  "cl" 'evilnc-quick-comment-or-uncomment-to-the-line
  "cc" 'evilnc-copy-and-comment-lines
  "cp" 'evilnc-comment-or-uncomment-paragraphs
  "cr" 'comment-or-uncomment-region
  "cv" 'evilnc-toggle-invert-comment-line-by-line
  ",," 'evilnc-comment-operator)
