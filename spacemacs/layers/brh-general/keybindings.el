;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Saved Macros
;; Replace a markdown inline link with a link reference
(fset 'brh/markdown-link-replace
      [?y ?% ?G ?o escape ?p ?a ?: ?  escape ?\C-o ?\C-o ?% ?l ?d ?% ?G ?$ ?p ?d ?s ?\) ?\C-o ?\C-o])

; Swaps MM/DD/YYYY -> YYYY/MM/DD. Useful for importing financial transactions into ledger.
(fset 'brh/dateswap-to-YYYY/MM/DD
      [?E ?b ?h ?x ?d ?e ?B ?P ?a ?/ escape ?B])

; Swaps YYYY/MM/DD -> MM/DD/YYYY. Inverse of the above function.
(fset 'brh/dateswap-to-MM/DD/YYYY
      [?l ?B ?d ?t ?/ ?E ?a ?/ escape ?p ?B ?x])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Global keybindings
(global-set-key (kbd "C-h") 'evil-window-left)
(global-set-key (kbd "C-j") 'evil-window-down)
(global-set-key (kbd "C-k") 'evil-window-up)
(global-set-key (kbd "C-l") 'evil-window-right)

(global-set-key (kbd "<C-up>") 'shrink-window)
(global-set-key (kbd "<C-down>") 'enlarge-window)
(global-set-key (kbd "<C-left>") 'shrink-window-horizontally)
(global-set-key (kbd "<C-right>") 'enlarge-window-horizontally)

(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cl" 'org-store-link)

(global-set-key (kbd "<f4>") 'brh/diff-head)
(global-set-key (kbd "<f5>") 'brh/goto-weekly-review)
(global-set-key (kbd "<f7>") 'brh/timestamp-for-clock)
(global-set-key (kbd "S-<f7>") 'brh/search-and-timestamp-for-clock)
(global-set-key (kbd "<f8>") 'org-agenda)
(global-set-key (kbd "<f9>") 'org-clock-goto)
(global-set-key (kbd "<f10>") 'brh/org-clock-in-with-prefix)
(global-set-key (kbd "<f11>") 'helm-org-rifle)
(global-set-key (kbd "S-<f11>") 'brh/search-and-clock-in)
(global-set-key (kbd "<f12>") 'brh/smart-agenda)

(spacemacs/set-leader-keys
  "dom"  'brh/diff-origin-master
  "jj"   'dumb-jump-go
  "jk"   'dumb-jump-back
  "oa"   'org-agenda
  "oba"  'brh/org-agenda-write
  "obgc" 'org-capture-goto-last-stored
  "obgr" 'org-refile-goto-last-stored
  "obgw" 'brh/goto-weekly-review
  "obh"  'brh/ledger-habit
  "obw"  'brh/goto-weekly-review
  "oc"   'org-capture
  "od"   'brh/diff-ref
  "of"   'magit-pull-from-upstream
  "og"   (lambda () "Git grep repository from root" (interactive)
           (let ((current-prefix-arg '(4)))
             (call-interactively 'helm-grep-do-git-grep)))
  "oml"  'brh/markdown-link-replace
  "oo"   'brh/smart-todo
  "op"   'magit-push-current-to-upstream
  "os"   'org-sort-entries
  "ot"   (lambda () "Add org TODO" (interactive)
           (org-insert-todo-heading-respect-content))
  "ovd"  'alsamixer-down-volume
  "ovm"  'alsamixer-toggle-mute
  "ovu"  'alsamixer-up-volume
  "ow"   'brh/smart-todo
  "rc"   'brh/tmux-run-clear
  "rl"   'brh/tmux-run-line
  "rL"   'brh/tmux-run-line-clear
  "rr"   'brh/tmux-repeat
  "rt"   'brh/helm-run-shell
  "rT"   (lambda () (interactive) (find-file "~/dotfiles_local/emacs_local/shell-cmds")))
