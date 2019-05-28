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

(defun brh/current-line-empty-p ()
  (looking-at "^$"))

(defun brh/timestamp-for-clock ()
  "Interactively create a CLOCK: timestamp entry by asking for start/finish"
  (interactive)
  (beginning-of-line)
  ; Allow running from the middle of a clock table
  (while (or (looking-at "CLOCK:") (looking-at ":END:")) (forward-line -1))
  (when (not (brh/current-line-empty-p)) (evil-open-below 1))
  (insert "CLOCK: ")
  (org-time-stamp-inactive)
  (insert "--")
  (org-time-stamp-inactive)
  (org-clock-update-time-maybe)
  (beginning-of-line))

(defun brh/org-clock-in-with-prefix ()
  "Clock in, setting the prefix argument to show recent clocks for selection"
  (interactive)
  (progn
    (call-interactively 'org-mru-clock-in)
    (org-clock-goto)))

(defun brh/clock-in-task-home-journal ()
  (interactive)
  (bh/clock-in-task-by-id "bb32a90c-2948-4661-aa09-6a343c2d9764"))

(defun brh/clock-in-task-home-planning-and-organization ()
  (interactive)
  (bh/clock-in-task-by-id "f3232aa5-d43b-4147-91d8-56e93f17a6e5"))

(defun brh/clock-in-task-home-rest-and-relaxation ()
  (interactive)
  (bh/clock-in-task-by-id "c63153ab-4b04-410b-a236-d5a6656b6f21"))

(defun brh/clock-in-task-home-sleeping ()
  (interactive)
  (bh/clock-in-task-by-id "4b493e83-bdaa-4f38-9b33-088682d1cd43"))

(defun brh/goto-weekly-review ()
  "Go to the most recent weekly review"
  (interactive)
  (org-id-goto "9acee905-3db5-4cff-9526-928e9693a323")
  (outline-show-subtree)
  (org-down-element)
  (dotimes (i 4) (org-forward-element))
  (recenter))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; bh/ functions are taken from http://doc.norang.ca/org-mode.html#License
;;
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

(defun bh/clock-in-task-by-id (id)
  "Clock in a task by id and jump to it in the buffer"
  (org-with-point-at (org-id-find id 'marker) (org-clock-in nil))
  (org-clock-goto))

)
