(with-eval-after-load 'org
  ;; Org variable configuration

  ;; Hook to tell if I'm at home or at work, to be re-used by other functions
  (setq brh/at-work (file-exists-p "~/dotfiles_local/notes/work.org"))

  ;; Enable keybindings defined below for TODO selection
  (setq org-use-fast-todo-selection t)

  ;; Log timestamps and state transitions into the LOGBOOK drawer
  (setq org-log-into-drawer t)

  ;; The above already captures this information
  (setq org-log-done nil)

  ;; Default TODO progression sequence.
  (setq org-todo-keywords '((sequence "TODO(t)" "BLOCKED(b@)" "WAITING(w@)" "PR(p@)" "|" "DONE(d!)" "ABANDONED(a@)")))

  ;; Use pinned reveal.js
  (setq org-reveal-root "file:///home/bhipple/dotfiles/reveal.js")

  (setq org-lowest-priority ?F)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Exporting Settings
  ;; See http://orgmode.org/manual/Export-settings.html for more details
  (setq org-export-with-clocks t)
  (setq org-export-with-drawers t)
  (setq org-export-with-properties t)
  (setq org-export-with-priority t)

  ;: Line breaks seem to get messed up regardless of whether this is T or F
  ;(setq org-export-preserve-breaks t)

  ;; Leave previously unopened buffers open after a search, to speed up subsequent searches
  (setq helm-org-rifle-close-unopened-file-buffers nil)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Tags
  (setq org-tag-alist '(("AWS" . ?a)
                        ("CRYPTO" . ?c)
                        ("DEEP" . ?d)
                        ("EMACS" . ?e)
                        ("FINANCE" . ?f)
                        ("HEALTH" . ?h)
                        ("JIRA" . ?j)
                        ("OKR" . ?k)
                        ("LEDGER" . ?l)
                        ("NIX" . ?n)
                        ("ORG" . ?o)
                        ("PROJECT" . ?p)
                        ("READING" . ?r)
                        ("SOMEDAY" . ?s)
                        ("TECH" . ?t)
                        ("WORK" . ?w)
                        ("ZSH" . ?z)))

  ;; These tags should never be inherited for the purpose of property searches/filters
  (setq org-tags-exclude-from-inheritance '("PROJECT"))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Org Agenda settings
  ;; Dynamically find all agenda files by walking the directory
  (setq org-agenda-files (delete-dups (brh/find-org-file-recursively "~/org/" "org")))

  ;; How far in advance to show deadlines on agenda views
  (setq org-deadline-warning-days 10)

  ;; Agenda clock report parameters
  (setq org-agenda-clockreport-parameter-plist
        '(:link f :maxlevel 5 :fileskip0 t :compact t :narrow 100))

  ;; By default, start up with log-mode turned on
  (setq org-agenda-start-with-log-mode t)

  ;; Don't give a warning color to tasks with impending deadlines if they are
  ;; scheduled to be done
  (setq org-agenda-skip-deadline-prewarning-if-scheduled (quote pre-scheduled))

  ;; Sort tasks in order of when they are due and then by priority
  (setq org-agenda-sorting-strategy
        (quote
         ((agenda deadline-up priority-down)
          (todo priority-down category-keep)
          (tags priority-down category-keep)
          (search category-keep))))

  ;; Set all of the check parameters for clock consistency checks. These are
  ;; currently all the default, except for max-duration, which I've bumped up to
  ;; 12 hours for those days when I sleep a lot.
  (setq org-agenda-clock-consistency-checks
        '(:max-duration "12:00"
         :min-duration 0
         :max-gap "0:05"
         :gap-ok-around ("4:00")
         :default-face
         ((:background "DarkRed")
         (:foreground "white"))
         :overlap-face nil :gap-face nil :no-end-time-face nil :long-face nil :short-face nil))

  ;; Tags for inactive tasks
  (setq brh-org-non-actionable "-SOMEDAY-TODO=\"BLOCKED\"-TODO=\"WAITING\"-TODO=\"PR\"-TODO=\"ABANDONED\"-TODO=\"DONE\"")

  ;; Org Agenda custom searches
  (setq org-agenda-custom-commands
        '(("b" "Blocked and Waiting items" ((tags-todo "TODO=\"BLOCKED\"|TODO=\"WAITING\"|TODO=\"PR\"")))
          ("c" "Currently active non-repeating items" tags-todo (concat "-REPEATING" brh-org-non-actionable))
          ("h" . "HOME searches")
          ("hh" "All HOME items" tags-todo "HOME")
          ("ha" "Today's agenda and HOME items" ((agenda "" ((org-agenda-span 'day)
                                                             (org-agenda-archives-mode t)
                                                             (org-agenda-log-mode-items '(closed clock state))))
                                                 (tags-todo (concat "+HOME-INBOX-REPEATING" brh-org-non-actionable))
                                                 (tags-todo "+INBOX")
                                                 (tags-todo "+REPEATING")
                                                 nil
                                                 ("/tmp/hipple/home_agenda.html")
                                                 ))
          ("hc" "Currently active non-repeating HOME items" tags-todo (concat "+HOME-REPEATING-INBOX" brh-org-non-actionable))
          ("hs" "Search HOME items" ((tags "+HOME") (search "")))
          ("n" "Today's agenda and all TODO items" ((agenda "" ((org-agenda-span 'day)
                                                                (org-agenda-log-mode-items '(closed clock state))))
                                                    (tags-todo "-SOMEDAY+TODO=\"TODO\"")))
          ("p" . "Project searches")
          ("pa" "Project List (All)" ((tags "+PROJECT-TODO=\"DONE\"-TODO=\"ABANDONED\"") (org-agenda-use-tag-inheritance nil)))
          ("pl" "Project List (Active)" ((tags "+PROJECT-SOMEDAY-TODO=\"DONE\"-TODO=\"ABANDONED\"") (org-agenda-use-tag-inheritance nil)))
          ("r" "Weekly Review" agenda "" ((org-agenda-span 'week)
                                          (org-agenda-log-mode-items '(closed clock state))
                                          (org-agenda-archives-mode t)))
          ("w" . "WORK searches")
          ("wa" "Today's agenda and WORK items" ((agenda "" ((org-agenda-span 'day)
                                                             (org-agenda-archives-mode t)
                                                             (org-agenda-log-mode-items '(closed clock state))))
                                                 (tags-todo (concat "-INBOX" brh-org-non-actionable))
                                                 (tags-todo "+INBOX"))
                                                 ((org-agenda-tag-filter-preset '("+WORK"))))
          ("wb" "WORK Blocked and Waiting items" ((tags-todo "+WORK+TODO=\"BLOCKED\"|+WORK+TODO=\"WAITING\"|+WORK+TODO=\"PR\"")))
          ("wc" "Currently active non-repeating WORK items" tags-todo "+WORK-SOMEDAY-REPEATING")
          ("we" "Work agenda for exporting to html" ((agenda "" ((org-agenda-span 'day)
                                                                 (org-agenda-archives-mode t)
                                                                 (org-agenda-log-mode-items '(closed clock state))))
                                                     (tags-todo "-TODO=\"BLOCKED\"-TODO=\"WAITING\""))
                                                     ((org-agenda-tag-filter-preset '("+WORK"))))
          ("ws" "Search WORK items" ((tags-todo "+WORK") (search "")))
          ("ww" "All WORK items" tags-todo "+WORK")))

  ;; Org super-agenda groupings
  ;; Some things to note about this:
  ;; 1. "Other items" collects everything not matched, and has default order 99.
  ;; 2. Tasks match group selectors top-to-bottom, and are consumed by the first match.
  ;; For full documentation: https://github.com/alphapapa/org-super-agenda
  (setq org-super-agenda-groups '(
     (:name "Today"
      :time-grid t
      :log t)

     (:name "Habits"
      :habit t
      :order 1000)

     (:name "Scheduled and Deadlines"
      :scheduled t
      :deadline t)

     (:name "Important"
      :priority "A")

     (:name "Quick wins"
      :effort< "0:31")

     (:name "Long tasks"
            :effort> "1:01")

     ; These categories should all appear after "Other Items"
     (:order-multi (100
       (:tag ("DEEP"))

       (:name "Emacs and Org"
        :tag ("EMACS" "ORG"))

       (:name "Finance"
        :tag ("FINANCE"))
     ))))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Org capture templates
  (setq brh/default-org-file (if brh/at-work "~/org/work/work.org" "~/org/me.org"))
  (setq brh/journal-file (if brh/at-work "~/org/work/journal.org" "~/personal/journal.org"))

  (setq org-capture-templates
        ;; Personal templates
        '(("b" "Buy Item" entry (file+headline "~/org/lists.org" "Shopping List")
           "* %?\nEntered %u\n")
          ("d" "Deadline Item" entry (file+headline "~/org/me.org" "Tasks")
           "* TODO [#C] %?\nDEADLINE: %^t")
          ("i" "Inbox TODO" entry (file+headline brh/default-org-file "Inbox")
           "* TODO [#C] %?\nEntered %u\n")
          ("j" "Journal Entry" entry (file+datetree brh/journal-file)
           (function _brh/org-capture-journal)
           :immediate-finish t
           :jump-to-captured t)
          ("m" "Someday/Maybe Item" entry (file+headline brh/default-org-file "Someday / Maybe")
           "* TODO [#C] %?\nEntered %u\n")
          ("r" "Weekly Review" entry (file "~/org/logs.org")
           (function _brh/org-capture-weekly-review))
          ("s" "Scheduled Item" entry (file+headline "~/org/me.org" "Tasks")
           "* TODO [#C] %?\nSCHEDULED: %^t")
          ("t" "Standard Todo" entry (file+headline brh/default-org-file "Tasks")
           "* TODO [#C] %?\nEntered %u\n")))

  ;; Default notes file for capture
  (setq org-default-notes-file "~/org/me.org")

  ;; Set org-refile to autocomplete three levels deep and check all agenda files
  (setq org-refile-targets '((org-agenda-files . (:maxlevel . 3))))

  ;; Archive to subdirectory
  (setq org-archive-location "~/org/archive/%s_archive::")

  (setq org-src-fontify-natively t)
  (setq org-src-tab-acts-natively nil)

  ;; global Effort estimate values
  (setq org-global-properties (quote (("Effort_ALL" . "0:15 0:30 1:00 2:00 3:00 4:00 5:00 6:00 0:00")
                                      ("Depth_ALL" . "Light Deep"))))

  ;; If clocking out on a zero-time task, delete the clock entry entirely
  (setq org-clock-out-remove-zero-time-clocks t)

  ;; Persist the clock through emacs reboots
  (setq org-clock-persist t)

  ;; Do not prompt to resume an active clock
  (setq org-clock-persist-query-resume nil)

  ;; Enable auto clock resolution for finding open clocks
  (setq org-clock-auto-clock-resolution (quote when-no-clock-is-running))

  ;; Allow clocking in to resume tasks with open clocks
  (setq org-clock-in-resume t)

  ;; Keep more than the default 5 entries
  (setq org-clock-history-length 35)

  ;; Change tasks to TODO when clocking in
  (setq org-clock-in-switch-to-state 'bh/clock-in)

  ;; State transitions that will trigger a clock-out
  (setq org-clock-out-when-done '("DONE" "WAITING" "BLOCKED" "PR"))

  ;; Always keep clocksum formatted in hours and minutes; never days.
  (setq org-time-clocksum-format
        '(:hours "%d" :require-hours t :minutes ":%02d" :require-minutes t))

  ;; Don't let me delete/modify invisible blocks
  (setq org-catch-invisible-edits 'error)

  ;; Link by IDs, not filename:header
  (setq org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)

  ;; And don't preserve IDs when cloning
  (setq org-clone-delete-id t)

  ;; When in org-mode, automatically word wrap text on the line limit
  (add-hook 'org-mode-hook #'spacemacs/toggle-auto-fill-mode-on)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Org-habit configuration
  ;; On multi-day agenda pages, show habits done/scheduled for all days
  (setq org-habit-show-habits-only-for-today nil)

  ;; Move the habits more to the right than the default (40)
  (setq org-habit-graph-column 80)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; org-roam configuration
  (setq org-roam-v2-ack t)
  (setq org-roam-directory (file-truename "~/org"))

  ; This kicks on every org mode file save, and used to cause some lag, but it
  ; is really convenient. Upstream performance regressions have been fixed.
  (setq org-roam-db-update-on-save t)

  (org-roam-db-autosync-mode nil)

  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t)

  (add-to-list 'display-buffer-alist
               '("\\*org-roam\\*"
                 (display-buffer-in-side-window)
                 (dedicated . t)
                 (side . right)
                 (slot . 0)
                 (window-width . 0.33)
                 (window-parameters . ((no-other-window . t)
                                       (no-delete-other-windows . t)))))

  (defun my-org-roam-node-visit ()
    (interactive)
    (org-roam-node-visit (org-roam-node-at-point t) t t))
  (define-key org-roam-node-map [remap org-roam-buffer-visit-thing] 'my-org-roam-node-visit)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Elfeed config
  (setq-default elfeed-search-filter "@1-month-ago +unread ")

)
