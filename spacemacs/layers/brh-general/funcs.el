;; General emacs functions

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Misc
;; Yanked from https://github.com/alphapapa/unpackaged.el#ensure-blank-lines-between-headings-and-before-contents
(defun unpackaged/org-sort-multi (keys)
  "Call `org-sort-entries' with multiple sorting methods specified in KEYS."
  ;; Message copied from `org-sort-entries'.
  (interactive (list (read-string "Sort by: [a]lpha  [n]umeric  [p]riority  p[r]operty  todo[o]rder  [f]unc
         [t]ime [s]cheduled  [d]eadline  [c]reated  cloc[k]ing
         A/N/P/R/O/F/T/S/D/C/K means reversed: ")))
  (seq-do (lambda (key)
            (org-sort-entries nil key))
          (nreverse keys)))

(defun brh/shell-region (start end)
  "Execute region in an inferior shell"
  (interactive "r")
  (shell-command (buffer-substring-no-properties start end)))

(defun brh/sh-lines (cmd)
  "Run a shell cmd and return its output lines as a list of strings, omitting nulls"
  (interactive)
  (split-string (shell-command-to-string cmd) "\n" t))

(defun brh/projectile-init ()
  "Mass initialize projectile caches for all git repos in ~/git and ~/src"
  (interactive)
  (mapc 'projectile-add-known-project
      (brh/sh-lines "find ~/git/ ~/src/ -maxdepth 3 -name '.git' | grep -v dotfiles | sed 's|/.git||'")))

(defun brh/read-file-to-list (fname)
  "Read a file and return a list of strings for the lines"
  (with-temp-buffer
    (insert-file-contents fname)
    (split-string (buffer-string) "\n" t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dash Docs
(defun brh/install-docsets ()
  "Install all of my chosen dash docsets"
  (interactive)
  (mapcar 'dash-docs-install-docset brh/dash-docsets))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Git Diff
(defun brh/_diff-buffer (ref)
  "Diff the current buffer against ref"
  (save-buffer 0)
  (vc-version-ediff (list buffer-file-name) ref ""))

(defun brh/diff-origin-master ()
  "Diff the current buffer against origin/master"
  (interactive)
    (brh/_diff-buffer "origin/master"))

(defun brh/diff-origin-dev ()
  "Diff the current buffer against origin/dev"
  (interactive)
  (brh/_diff-buffer "origin/dev"))

(defun brh/diff-head ()
  "Diff the current buffer against HEAD"
  (interactive)
  (brh/_diff-buffer "HEAD"))

(defun brh/diff-ref ()
  "Diff the current buffer against an entered ref"
  (interactive)
  (brh/_diff-buffer (read-string "Ref to diff against: ")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ansi-term
;; Evil mode is generally desirable, but it will eat a few key bindings. Here we
;; setup some shell hook bindings to preserve those.
(defun brh/send-C-d ()
  (term-send-raw-string "\C-d"))

(defun brh/send-C-r ()
  (term-send-raw-string "\C-r"))

(defun brh/send-C-space ()
  (term-send-raw-string "\C-@"))

(defun brh/setup-term-mode ()
  (progn
    ; Maintain C-d, which is useful for killing interactive REPLs
    ; TODO: This one doesn't work yet?
    (evil-local-set-key 'insert (kbd "C-d") 'brh/send-C-d)
    ; Maintain C-r, which is useful for FZF activations and history
    (evil-local-set-key 'insert (kbd "C-r") 'brh/send-C-r)
    ; Maintain C-SPC, which is useful for zsh autosuggestions completion
    (evil-local-set-key 'insert (kbd "C-SPC") 'brh/send-C-space)))

(add-hook 'term-mode-hook 'brh/setup-term-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tmux
; Save the previous shell cmd that I ran for convenience in tmux-repeat
(setq brh/last-shell-cmd "")

(defun brh/_tmux-cmd (cmd)
  "Send a command to the active tmux terminal session. Also saves the buffer"
  (save-buffer 0)
  (shell-command (concat "tmux send-keys '" cmd "' Enter")))

(defun brh/helm-run-shell ()
  "Interactively select a cmd to run in the shell with tmux"
  (interactive)
  (let* ((cmd-file (expand-file-name "~/dotfiles_local/emacs_local/shell-cmds"))
         (cmds (brh/read-file-to-list cmd-file))
         (sel (helm-comp-read
               "shell command: " cmds)))
    ; If we entered something new in the helm menu, add it to the list for the future
    (when (not (member sel cmds))
      (write-region (concat "\n" sel) nil cmd-file 'append))
    (setq brh/last-shell-cmd sel)
    (brh/_tmux-cmd sel)))

(defun brh/tmux-repeat ()
  "Repeat the previous command in the active terminal session"
  (interactive)
  (brh/_tmux-cmd brh/last-shell-cmd))

;; TODO: Work around the fact that the buffer is read-only, and also make sure
;; that this incremental buffer fetch really works as expected.
;; (defun brh/helm-run-ansi-shell ()
;;   (interactive)
;;   (if (get-buffer "*ansi-term-1*")
;;     (switch-to-buffer-other-window "*ansi-term-1*")
;;     (spacemacs/default-pop-shell))
;;   (evil-append-line 1)
;;   (insert "foobar"))


(defun elfeed-mark-all-as-read ()
  "Mark all unread elfeed items as read"
  (interactive)
  (mark-whole-buffer)
  (elfeed-search-untag-all-unread))
