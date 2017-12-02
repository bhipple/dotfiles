(with-eval-after-load 'org
  (defun brh/find-org-file-recursively (&optional directory filext)
    "Return .org and .org_archive files recursively from DIRECTORY.
     If FILEXT is provided, return files with extension FILEXT instead."
    (interactive "Org Directory: ")
    (let* (org-file-list
     (case-fold-search t)       ; filesystems are case sensitive
     (file-name-regex "^[^.#].*") ; exclude dot, autosave, and backup files
     (filext (or filext "org$\\\|org_archive"))
     (fileregex (format "%s\\.\\(%s$\\)" file-name-regex filext))
     (cur-dir-list (directory-files directory t file-name-regex)))
      ;; loop over directory listing
      (dolist (file-or-dir cur-dir-list org-file-list) ; returns org-file-list
        (cond
         ((file-regular-p file-or-dir) ; regular files
    (if (string-match fileregex file-or-dir) ; org files
        (add-to-list 'org-file-list file-or-dir)))
         ((file-directory-p file-or-dir)
    (dolist (org-file (brh/find-org-file-recursively file-or-dir filext)
          org-file-list) ; add files found to result
      (add-to-list 'org-file-list org-file)))))))

(defun brh/smart-agenda ()
  "Show my work agenda if the work.org file exists, and my home agenda otherwise."
  (interactive)
  (if (file-exists-p "~/org/work/work.org")
      (org-agenda nil "wa")
      (org-agenda nil "ha")))

;; Not working yet; trying to convert a date like 4/27/2014 => [2014-04-27] for importing old docs in org mode.
;; TODO: Figure out how to take the visual region selection, feed it to this, and replace the text with the output.
(defun brh/convert-date (time-string)
  (interactive "s")
  (let* ((time (parse-time-string time-string))
         (day (nth 3 time))
         (month (nth 4 time))
         (year (nth 5 time)))
    (format-time-string "[%Y-%m-%d]"
                        (encode-time 0 0 0 day month year))))

;; bh/ functions are taken from http://doc.norang.ca/org-mode.html#License
(defun bh/is-project-p ()
  "Any task with a todo keyword subtask"
  (save-restriction
    (widen)
    (let ((has-subtask)
          (subtree-end (save-excursion (org-end-of-subtree t)))
          (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
      (save-excursion
        (forward-line 1)
        (while (and (not has-subtask)
                    (< (point) subtree-end)
                    (re-search-forward "^\*+ " subtree-end t))
          (when (member (org-get-todo-state) org-todo-keywords-1)
            (setq has-subtask t))))
      (and is-a-task has-subtask))))

(defun bh/is-project-subtree-p ()
  "Any task with a todo keyword that is in a project subtree.
Callers of this function already widen the buffer view."
  (let ((task (save-excursion (org-back-to-heading 'invisible-ok)
                              (point))))
    (save-excursion
      (bh/find-project-task)
      (if (equal (point) task)
          nil
        t))))

(defun bh/is-task-p ()
  "Any task with a todo keyword and no subtask"
  (save-restriction
    (widen)
    (let ((has-subtask)
          (subtree-end (save-excursion (org-end-of-subtree t)))
          (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
      (save-excursion
        (forward-line 1)
        (while (and (not has-subtask)
                    (< (point) subtree-end)
                    (re-search-forward "^\*+ " subtree-end t))
          (when (member (org-get-todo-state) org-todo-keywords-1)
            (setq has-subtask t))))
      (and is-a-task (not has-subtask)))))

(defun bh/is-subproject-p ()
  "Any task which is a subtask of another project"
  (let ((is-subproject)
        (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
    (save-excursion
      (while (and (not is-subproject) (org-up-heading-safe))
        (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
          (setq is-subproject t))))
    (and is-a-task is-subproject)))

(defun bh/clock-in (kw)
  "If task was in a blocked state, clocking in switches it to TODO"
  (if (member kw org-clock-out-when-done)
    "TODO"
    kw))
)
