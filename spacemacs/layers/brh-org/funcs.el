(with-eval-after-load 'org
  (defun brh/org-agenda-file-filter (fname)
    "Return true if fname should be included in org-agenda-files. Also runs on
     dirs and short circuits filesystem walk."
    (not (or
          (string-match "/presentations/" fname)
          (string-match "/journal.org" fname)
          )))

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
    (if (and (string-match fileregex file-or-dir) (brh/org-agenda-file-filter file-or-dir)) ; org files
        (add-to-list 'org-file-list file-or-dir)))
         ((file-directory-p file-or-dir)
    (dolist (org-file (brh/find-org-file-recursively file-or-dir filext)
          org-file-list) ; add files found to result
      (add-to-list 'org-file-list (file-truename org-file))))))))

(defun brh/smart-agenda ()
  "Show my work agenda if at work, and my home agenda otherwise."
  (interactive)
  (if brh/at-work
      (org-agenda nil "wa")
      (org-agenda nil "ha")))

(defun brh/smart-todo ()
  "Go to work.org if at work, otherwise me.org"
  (interactive)
  (if brh/at-work
      (find-file "~/dotfiles_local/notes/work.org")
      (find-file "~/personal/me.org")))

(defun brh/org-agenda-write ()
  "Write the current agenda to the Chromium homepage file"
  (interactive)
  (make-directory "/home/bhipple/public_html/org" t)
  (org-agenda-write "/home/bhipple/public_html/org/review.html"))

(defun brh/current-line-empty-p ()
  (looking-at "^$"))

(defun brh/timestamp-for-clock ()
  "Interactively create a CLOCK: timestamp entry by asking for start/finish"
  (interactive)
  (beginning-of-line)
  (widen)
  (while (not (looking-at "^*"))
    (org-up-element))
  (outline-show-subtree)
  ; Search within the element for a LOGBOOK and go to it; throw an exception if missing
  (re-search-forward "^:LOGBOOK:" (save-excursion (org-end-of-subtree)))
  ; Cycle through to make sure the LOGBOOK drawer is opened
  (org-down-element) (org-up-element)
  (recenter)
  (evil-open-below 1)
  (insert "CLOCK: ")
  (org-time-stamp-inactive)
  (insert "--")
  (org-time-stamp-inactive)
  (org-clock-update-time-maybe)
  (beginning-of-line))

(defun brh/search-and-timestamp-for-clock ()
  "Search for an org task with helm-org-rifle, then insert a timestamp interactively"
  (interactive)
  (helm-org-rifle)
  (brh/timestamp-for-clock))

(defun brh/search-and-clock-in ()
  "Search for an org task, then clock into it"
  (interactive)
  (helm-org-rifle)
  (org-clock-in))

(defun brh/org-clock-in-with-prefix ()
  "Clock in, setting the prefix argument to show recent clocks for selection"
  (interactive)
  (progn
    (call-interactively 'org-mru-clock-in)
    (org-clock-goto)))

(defun brh/goto-weekly-review ()
  "Go to the most recent weekly review"
  (interactive)
  (org-id-goto "9acee905-3db5-4cff-9526-928e9693a323")
  (outline-show-subtree)
  (org-down-element)
  (dotimes (i 4) (org-forward-element))
  (recenter))

(defun brh/org-roam-db-reset ()
  "Drop the roam db and resync it"
  (interactive)
  (progn
    (org-roam-db-clear-all)
    (org-roam-db-sync)))

(defun brh/link-journal-entries ()
  "Link all of my journal entries with org IDs"
  (interactive)
  (let* (
         (journal-id "bb32a90c-2948-4661-aa09-6a343c2d9764")
         )
    )
  (progn
    (show-subtree)
    (while (re-search-forward "\* 20")
      (org-id-get-create)
      (message (thing-at-point 'line))
      )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org functions for org-capture-templates.
; Note that org-capture takes the function by the string name and then passes it
; to `funcall`, so we have to actually have a defined function, not just a
; lambda.
(defun _brh/org-capture-journal ()
  (progn "* %U\n"))

(defun _brh/org-capture-weekly-review ()
  "Helper function to go to the weekly review, clock in, and expand my snippet"
  (let* (
         ; TODO: This should be last-review-date +1 so that it doesn't double-count the boundary day itself
         (last-review-date (string-trim (shell-command-to-string "grep 'Weekly Review for ' /home/bhipple/personal/roam/logs.org | head -1 | awk '{print $5}' | sed 's|\\[||'")))
         (ledger #'(lambda (rest) (string-trim (shell-command-to-string (concat "lp -b " last-review-date " " rest)))))
         (weekly-review-id "9acee905-3db5-4cff-9526-928e9693a323")
         (today (format-time-string "%Y-%m-%d %a"))
        )
    (progn
    (bh/clock-in-task-by-id weekly-review-id)
    (org-id-goto weekly-review-id)
    (show-subtree)
    (re-search-forward "^** Weekly Review for ")
    (forward-line -1)
    (insert "
** Weekly Review for [" today "]
*** Recap of last week
Meditation Sessions: " (funcall ledger "r Meditation | wc -l") "
Lifts: " (string-trim (shell-command-to-string (concat "workouts.py" " --begin " last-review-date " --count"))) "
Running: " (funcall ledger "r Running | wc -l") "
Fasts: " (funcall ledger "r Fasting | wc -l") "
RLT: " (funcall ledger "r RLT | wc -l") "
NF: " (funcall ledger "b NF | awk '{print $1}'") "
*** Goals for next week
- [ ]
- [ ]
- [ ]
*** Lessons learned
**** What went well
**** What didn't go well
**** What to change
")
    (re-search-backward "^** Weekly Review for ")
    (setq-local this-id (org-id-get-create))
    (recenter))))

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
