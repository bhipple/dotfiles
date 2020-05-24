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

(defun brh/sort-section ()
  "Sort the lines in a paragraph"
  (interactive)
  (execute-kbd-macro 'brh/visual-paragraph)
  (command-execute 'sort-lines))

(defun brh/number-list (times)
  "Insert the numbers 1 through N on lines"
  (interactive "sInsert numbers 1 through N for N = ")
  (dotimes (i (string-to-number times)) (insert (format "%d\n" (1+ i)))))

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

(defun brh/diff-head ()
  "Diff the current buffer against HEAD"
  (interactive)
  (brh/_diff-buffer "HEAD"))

(defun brh/diff-ref ()
  "Diff the current buffer against an entered ref"
  (interactive)
  (brh/_diff-buffer
   (helm-comp-read "Ref to diff against: " (cons "HEAD" (magit-list-refs)))))

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

(defun brh/get-tmux-pane ()
  "Get the number of the a tmux pane NOT running emacs on current window"
  (interactive)
  ; TODO: Replace all this nonsense using the shell command buffer with just:
  ; (replace-regexp-in-string"\n$" "" (shell-command-to-string "tmux list-panes ..."))

  (save-excursion
    (shell-command "tmux list-panes | grep -v active | tail -1 | cut -d':' -f1")
    ; TODO: There has to be a simpler way to get the buffer as a string!
    (switch-to-buffer "*Shell Command Output*" t t)
    (let ((res (thing-at-point 'word)))
      (kill-buffer "*Shell Command Output*")
      ; TODO: Attempt to just return 0 if we didn't find a valid non-active tmux
      ; split, which implies that we have an X11 emacs on a single-pane window.
      ; Not really sure this is actually working.
      (if res res "0"))))

(defun brh/_tmux-cmd (cmd)
  "Send a command to the active tmux terminal session. Also saves the buffer"
  (save-buffer 0)
  ; TODO: Detect if spacemacs is running inside a terminal, and if so send it to
  ; a non-active tmux pane
  (let* ((target-pane (brh/get-tmux-pane))
         (full-cmd (concat "tmux send-keys -t " target-pane " '" cmd "' Enter")))
    (shell-command full-cmd t t)))

(defun brh/helm-run-shell ()
  "Interactively select a cmd to run in the emacs async shell"
  (interactive)
  (let* ((cmd-file (expand-file-name "~/dotfiles_local/emacs_local/shell-cmds"))
         (cmds (brh/read-file-to-list cmd-file))
         (sel (helm-comp-read
               "shell command: " cmds)))
    ; If we entered something new in the helm menu, add it to the list for the future
    (when (not (member sel cmds))
      (write-region (concat "\n" sel) nil cmd-file 'append))
    (setq brh/last-shell-cmd sel)
    (brh/preferred-shell-func sel)))

(defun brh/preferred-shell-func (cmd)
  "Run a md in my preferred shell. Changes based on context."
  ; (brh/_tmux-cmd cmd)
  ; (async-shell-command cmd)
  (run-in-vterm cmd)
)

(defun brh/tmux-run-clear ()
  "Send the clear command to the terminal"
  (interactive)
  (brh/_tmux-cmd "; clear"))

(defun brh/tmux-run-line ()
  "Run the current line the cursor is on in a tmux-shell. Trim a leading '$ ', if found"
  (interactive)
  (brh/_tmux-cmd (replace-regexp-in-string "^[$> ]*" ""
                   (s-trim (thing-at-point 'line t)))))

(defun brh/tmux-run-line-clear ()
  "Run the current line the cursor is on in a tmux-shell after clearning the terminal"
  (interactive)
  (brh/tmux-run-clear)
  (brh/tmux-run-line))

(defun brh/tmux-repeat ()
  "Repeat the previous command in the active terminal session"
  (interactive)
  (brh/_tmux-cmd brh/last-shell-cmd))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Yanked from https://www.reddit.com/r/emacs/comments/ft84xy/run_shell_command_in_new_vterm/
(defun run-in-vterm-kill (process event)
  "A process sentinel. Kills PROCESS's buffer if it is live."
  (let ((b (process-buffer process)))
    (and (buffer-live-p b)
         (kill-buffer b))))

(defun run-in-vterm (command)
  "Execute string COMMAND in a new vterm.

Interactively, prompt for COMMAND with the current buffer's file
name supplied. When called from Dired, supply the name of the
file at point.

Like `async-shell-command`, but run in a vterm for full terminal features.

The new vterm buffer is named in the form `*foo bar.baz*`, the
command and its arguments in earmuffs.

When the command terminates, the shell remains open, but when the
shell exits, the buffer is killed."
  (interactive
   (list
    (let* ((f (cond (buffer-file-name)
                    ((eq major-mode 'dired-mode)
                     (dired-get-filename nil t))))
           (filename (concat " " (shell-quote-argument (and f (file-relative-name f))))))
      (read-shell-command "Terminal command: "
                          (cons filename 0)
                          (cons 'shell-command-history 1)
                          (list filename)))))
  (with-current-buffer (vterm (concat "*" command "*"))
    (set-process-sentinel vterm--process #'run-in-vterm-kill)
    (vterm-send-string command)
    (vterm-send-return)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
