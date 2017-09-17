(with-eval-after-load 'org
  ;; Org variable configuration

  ;; Dynamically find all agenda files by walking the directory
  (setq org-agenda-files (append (brh/find-org-file-recursively "~/org/" "org")))

  ;; Enable keybindings defined below for TODO selection
  (setq org-use-fast-todo-selection t)

  ;; Log timestamps and state transitions into the LOGBOOK drawer
  (setq org-log-into-drawer t)

  ;; The above already captures this information
  (setq org-log-done nil)

  ;; Default TODO progression sequence.
  (setq org-todo-keywords '((sequence "NEXT(n)" "TODO(t)" "BLOCKED(b@)" "WAITING(w@)" "|" "DONE(d!)" "ABANDONDED(a@)")))

  ;; Use pinned reveal.js
  (setq org-reveal-root "file:///home/bhipple/dotfiles/reveal.js")

  ;; When exporting, preserve line breaks
  (setq org-export-preserve-breaks t)

  ;; Tags
  (setq org-tag-alist '(("ALGOS" . ?a)
                        ("CODING" . ?c)
                        ("EMACS" . ?e)
                        ("HASKELL" . ?h)
                        ("GYM" . ?g)
                        ("LISTS" . ?l)
                        ("NIX" . ?n)
                        ("PROJECT" . ?p)
                        ("READING" . ?r)
                        ("SOMEDAY" . ?s)
                        ("VIM" ? .v)
                        ("WORK" ? .w)
                        ("ZSH" ? .z)))

  ;; Org capture templates
  (setq org-capture-templates
        ;; Personal templates
        '(("b" "Buy Item" entry (file+headline "~/org/lists.org" "Shopping List")
           "* %?\nEntered %u\n")
          ("d" "Deadline Item" entry (file+headline "~/org/me.org" "Tasks")
           "* TODO [#C] %?\nDEADLINE: %^t")
          ("m" "Someday/Maybe Item" entry (file+headline "~/org/me.org" "Someday / Maybe")
           "* TODO [#C] %?\nEntered %u\n")
          ("s" "Scheduled Item" entry (file+headline "~/org/me.org" "Tasks")
           "* TODO [#C] %?\nSCHEDULED: %^t")
          ("t" "Standard Todo" entry (file+headline "~/org/me.org" "Tasks")
           "* TODO [#C] %?\nEntered %u\n")

          ;; Work.org templates
          ("n" "Work Note" entry (file+headline "~/org/work/work.org" "Work Notes")
           "* %?\nEntered %u\n")
          ("w" "Work Todo" entry (file+headline "~/org/work/work.org" "Work Tasks")
           "* TODO [#C] %?\nEntered %u\n")))

  ;; Default notes file for capture
  (setq org-default-notes-file "~/org/me.org")
  ;; Abbreviations for links
  (setq org-link-abbrev-alist '(("gmap" . "http://maps.google.com/maps?q=%s")
                                ("omap" . "https://www.openstreetmap.org/search?query=%s")))

  ;; Set org-refile to autocomplete three levels deep and check all agenda files
  (setq org-refile-targets '((org-agenda-files . (:maxlevel . 3))))

  ;; Archive to subdirectory
  (setq org-archive-location "~/org/archive/%s_archive::")

  ;; How far in advance to show deadlines on agenda views
  (setq org-deadline-warning-days 10)

  ;; By default, don't show DONE and archived items.
  (setq org-agenda-log-mode nil)
  (setq org-agenda-archives-mode nil)

  ;; Org Agenda custom searches
  (setq org-agenda-custom-commands
        '(("b" "Blocked and Waiting items" ((tags-todo "TODO=\"BLOCKED\"|TODO=\"WAITING\"")))
          ("c" "Currently active non-repeating items" tags-todo "-SOMEDAY-REPEATING")
          ("h" . "HOME searches")
          ("hh" "All HOME items" tags-todo "HOME")
          ("ha" "Today's agenda and HOME items" ((agenda "" ((org-agenda-span 'day)
                                                             (org-agenda-archives-mode nil)
                                                             (org-agenda-log-mode nil)))
                                                 (tags-todo "+HOME-TODO=\"BLOCKED\"-TODO=\"WAITING\"")))
          ("hc" "Currently active non-repeating HOME items" tags-todo "+HOME-SOMEDAY-REPEATING")
          ("hs" "Search HOME items" ((tags "+HOME") (search "")))
          ("n" "Today's agenda and all NEXT items" ((agenda "" ((org-agenda-span 'day)
                                                                (org-agenda-log-mode t)))
                                                    (tags-todo "-SOMEDAY+TODO=\"NEXT\"")))
          ("p" . "Projects and Priority searches")
          ("pa" "Priority A items" tags-todo "+PRIORITY=\"A\"")
          ("pb" "Priority B items" tags-todo "+PRIORITY=\"B\"")
          ("pc" "Priority C items" tags-todo "+PRIORITY=\"C\"")
          ("pl" "Project List" ((tags-todo "+PROJECT") (org-agenda-use-tag-inheritance nil)))
          ("r" "Weekly Review" agenda "" ((org-agenda-span 'week)
                                          (org-agenda-log-mode t)
                                          (org-agenda-archives-mode t)))
          ("w" . "WORK searches")
          ("wa" "Today's agenda and WORK items" ((agenda "" ((org-agenda-span 'day)
                                                             (org-agenda-archives-mode nil)
                                                             (org-agenda-log-mode nil)))
                                                 (tags-todo "+WORK-TODO=\"BLOCKED\"-TODO=\"WAITING\"")))
          ("wb" "WORK Blocked and Waiting items" ((tags-todo "+WORK+TODO=\"BLOCKED\"|+WORK+TODO=\"WAITING\"")))
          ("wc" "Currently active non-repeating WORK items" tags-todo "+WORK-SOMEDAY-REPEATING")
          ("we" "Work agenda for exporting to html" ((agenda "" ((org-agenda-span 'day)
                                                                 (org-agenda-archives-mode nil)
                                                                 (org-agenda-log-mode nil)))
                                                     (tags-todo "-HOME+WORK-TODO=\"BLOCKED\"-TODO=\"WAITING\"")))
          ("ws" "Search WORK items" ((tags-todo "+WORK") (search "")))
          ("ww" "All WORK items" tags-todo "WORK")))

  (setq org-src-fontify-natively t)

  ;; global Effort estimate values
  (setq org-global-properties (quote (("Effort_ALL" . "0:15 0:30 1:00 2:00 3:00 4:00 5:00 6:00 0:00")
                                      ("Depth_ALL" . "Light Deep"))))

  ;; If clocking out on a zero-time task, delete the clock entry entirely
  (setq org-clock-out-remove-zero-time-clocks t)

  ;; Persist the clock through emacs reboots
  (setq org-clock-persist t)

  ;; Change tasks to NEXT when clocking in
  (setq org-clock-in-switch-to-state 'bh/clock-in-to-next)

  ;; Always keep clocksum formatted in hours and minutes; never days.
  (setq org-time-clocksum-format
        '(:hours "%d" :require-hours t :minutes ":%02d" :require-minutes t))

  ;; Don't let me delete/modify invisible blocks
  (setq org-catch-invisible-edits 'error)

  ;; Link by IDs, not filename:header
  (setq org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)

  ;; And don't preserve IDs whe cloning
  (setq org-clone-delete-id t)

  (global-set-key "\C-ca" 'org-agenda)
  (global-set-key "\C-cb" 'org-iswitchb)
  (global-set-key "\C-cc" 'org-capture)
  (global-set-key "\C-cl" 'org-store-link)

  (global-set-key (kbd "<f12>") 'brh/smart-agenda)
  (global-set-key (kbd "<f8>") 'org-agenda)
  (global-set-key (kbd "<f9>") 'org-clock-goto)

)
