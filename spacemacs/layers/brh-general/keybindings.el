;; Global keybindings

(global-set-key (kbd "C-h") 'evil-window-left)
(global-set-key (kbd "C-j") 'evil-window-down)
(global-set-key (kbd "C-k") 'evil-window-up)
(global-set-key (kbd "C-l") 'evil-window-right)

(global-set-key (kbd "<C-up>") 'shrink-window)
(global-set-key (kbd "<C-down>") 'enlarge-window)
(global-set-key (kbd "<C-left>") 'shrink-window-horizontally)
(global-set-key (kbd "<C-right>") 'enlarge-window-horizontally)

(spacemacs/set-leader-keys
  "oa" 'org-agenda
  "ob" 'helm-buffers-list
  "oc" 'org-capture
  "od" (lambda () "EDiff with git revision" (interactive) (ediff-revision (buffer-file-name)))
  "of" 'magit-pull-from-upstream
  "og" (lambda () "Git grep repository from root" (interactive)
         (let ((current-prefix-arg '(4)))
           (call-interactively 'helm-grep-do-git-grep)))
  "oo" (lambda () (interactive) (find-file "~/org/me.org"))
  "op" 'magit-push-current-to-upstream
  "ot" (lambda () "Add org TODO" (interactive)
         (org-insert-todo-heading-respect-content))
  "os" 'org-sort-entries
  "ow" (lambda () (interactive) (find-file "~/dotfiles_local/notes/work.org"))
  )
