;; General emacs functions

(defun brh/shell-region (start end)
  "Execute region in an inferior shell"
  (interactive "r")
  (shell-command (buffer-substring-no-properties start end)))

(defun brh/cleanup-org-tables()
  "Replace org-table formatting with markdown formatting"
  (save-excursion
    (goto-char (point-min))
    (while (search-forward "-+-" nil t) (replace-match "-|-"))))
