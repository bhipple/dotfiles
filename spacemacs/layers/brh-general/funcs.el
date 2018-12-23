;; General emacs functions

(defun brh/shell-region (start end)
  "Execute region in an inferior shell"
  (interactive "r")
  (shell-command (buffer-substring-no-properties start end)))

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

(defun brh/_tmux-cmd (cmd)
  "Send a command to the active tmux terminal session. Also saves the buffer"
  (save-buffer 0)
  (shell-command (concat "tmux send-keys '" cmd "' Enter")))

(defun brh/tmux-run-terminal ()
  (interactive)
  (brh/_tmux-cmd (read-string "Command to run: ")))

(defun brh/tmux-repeat ()
  "Repeat the previous command in the active terminal session"
  (interactive)
  (brh/_tmux-cmd "jjk"))

(defun brh/tmux-run-waf ()
  (interactive)
  (brh/_tmux-cmd "wcb"))

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
