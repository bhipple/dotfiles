  ;; recursively find .org files in provided directory
  ;; modified from an Emacs Lisp Intro example
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

;; Show my work agenda if the work.org file exists, and my home agenda otherwise.
(defun brh/smart-agenda ()
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
