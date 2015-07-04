;;; dotfiles-tweaks --- Some extraneous functions for dev

;;; Commentary:
;;  Functions in here are largely to add or augment behavior

;;; Code:

;;; some generic-ish functions

;; (defun create-tags (dir-name)
;;   "Create tags file."
;;   (interactive "DDirectory: ")
;;   (shell-command
;;    (format "ctags -f tags -e -R %s --exclude=.svn --exclude=.git --exclude=node_modules --exclude=tmp *" (directory-file-name dir-name))))

(defun create-tags (dir-name)
  "Create tags file."
  (interactive "DDirectory: ")
  (message (format "%s -f %s/tags -eR %s"
                   path-to-ctags (directory-file-name dir-name)
                   (directory-file-name dir-name)))
  (shell-command
   (format "%s -f %s/tags -eR %s" path-to-ctags
           (directory-file-name dir-name) (directory-file-name dir-name))))


(defun open-init-file ()
  "Open default init file"
  (interactive)
  (find-file "~/.emacs.d/init.el") nil)

(defun ido-find-file-in-tag-files ()
  (interactive)
  (save-excursion
    (let ((enable-recursive-minibuffers t)) (visit-tags-table-buffer))
    (find-file (expand-file-name
                (ido-completing-read "Project file: "
                                     (tags-table-files) nil t)))))

;; indent whole buffer
(defun iwb()
  "Indent whole buffer"
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max)))

;; clear eshell buffer
(defun eshell/clear ()
  "Clear the eshell buffer"
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)))

(defun jump-to-end ()
  "Sets the cursor to the end-of-buffer and the beginning-of-line"
  (interactive)
  (goto-char (point-max))
  (end-of-line))


;; misc functions for killing the minibuffer with esc
(defun def-assoc (key alist default)
  "Return cdr of `KEY' in `ALIST' or `DEFAULT' if key is no car in alist."
  (let ((match (assoc key alist)))
    (if match
        (cdr match)
      default)))

(defun take (n lst)
  "Return atmost the first `N' items of `LST'."
  (let (acc '())
    (while (and lst (> n 0))
      (decf n)
      (push (car lst) acc)
      (setq lst (cdr lst)))
    (nreverse acc)))

(defun group (lst n)
  "Group `LST' into portions of `N'."
  (let (groups)
    (while lst
      (push (take n lst) groups)
      (setq lst (nthcdr n lst)))
    (nreverse groups)))

(defun pour-mappings-to (map mappings)
  "Calls `define-key' with `map' on every key-fun pair in `MAPPINGS'.
   `MAPPINGS' is a list of string-fun pairs, with a
   define-key-understandable string and a interactive-fun."
  (dolist (mapping (group mappings 2))
    (define-key map (car mapping) (cadr mapping)))
  map)

(defun fill-keymap (keymap &rest mappings)
  "Fill `KEYMAP' with `MAPPINGS'.
   See `pour-mappings-to'."
  (pour-mappings-to keymap mappings))

(defun fill-keymaps (keymaps &rest mappings)
  "Fill `KEYMAPS' with `MAPPINGS'. See `pour-mappings-to'."
  (dolist (keymap keymaps keymaps)
    (let ((map (if (symbolp keymap)
                   (symbol-value keymap)
                 keymap)))
      (pour-mappings-to map mappings))))

(defun minibuffer-keyboard-quit ()
  "Abort recursive edit.
   In Delete Selection mode, if the mark is active, just deactivate it;
   then it takes a second \\[keyboard-quit] to abort the minibuffer."
  (interactive)
  (if (and delete-selection-mode transient-mark-mode mark-active)
      (setq deactivate-mark  t)
    (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
    (abort-recursive-edit)))

(defun to-string (item)
  (if (symbolp item)
      (symbol-name (item))
    item))

(global-set-key (kbd "s-=") 'text-scale-increase)
(global-set-key (kbd "s--") 'text-scale-decrease)

(provide 'tweaks)
