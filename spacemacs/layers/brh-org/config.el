(with-eval-after-load 'org
  ;; Org variable configuration

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

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Exporting Settings
  ;; See http://orgmode.org/manual/Export-settings.html for more details
  (setq org-export-with-clocks t)
  (setq org-export-with-drawers t)
  (setq org-export-with-properties t)
  (setq org-export-with-priority t)

  ;: Line breaks seem to get messed up regardless of whether this is T or F
  ;(setq org-export-preserve-breaks t)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

  ;; These tags should never be inherited for the purpose of property searches/filters
  (setq org-tags-exclude-from-inheritance '("PROJECT"))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Org Agenda settings
  ;; Dynamically find all agenda files by walking the directory
  (setq org-agenda-files (append (brh/find-org-file-recursively "~/org/" "org")))

  ;; How far in advance to show deadlines on agenda views
  (setq org-deadline-warning-days 10)

  ;; Agenda clock report parameters
  (setq org-agenda-clockreport-parameter-plist
        '(:link f :maxlevel 5 :fileskip0 t :compact t :narrow 100))

  ;; By default, show archived items.
  (setq org-agenda-archives-mode t)

  ;; Org Agenda custom searches
  (setq org-agenda-custom-commands
        '(("b" "Blocked and Waiting items" ((tags-todo "TODO=\"BLOCKED\"|TODO=\"WAITING\"|TODO=\"PR\"")))
          ("c" "Currently active non-repeating items" tags-todo "-SOMEDAY-REPEATING")
          ("h" . "HOME searches")
          ("hh" "All HOME items" tags-todo "HOME")
          ("ha" "Today's agenda and HOME items" ((agenda "" ((org-agenda-span 'day)
                                                             (org-agenda-archives-mode t)
                                                             (org-agenda-log-mode t)
                                                             (org-agenda-log-mode-items '(closed clock state))))
                                                 (tags-todo "+HOME-TODO=\"BLOCKED\"-TODO=\"WAITING\"-SOMEDAY")
                                                 nil
                                                 ("/tmp/hipple/home_agenda.html")
                                                 ))
          ("hc" "Currently active non-repeating HOME items" tags-todo "+HOME-SOMEDAY-REPEATING")
          ("hs" "Search HOME items" ((tags "+HOME") (search "")))
          ("n" "Today's agenda and all TODO items" ((agenda "" ((org-agenda-span 'day)
                                                                (org-agenda-log-mode t)
                                                                (org-agenda-log-mode-items '(closed clock state))))
                                                    (tags-todo "-SOMEDAY+TODO=\"TODO\"")))
          ("p" . "Projects and Priority searches")
          ("pa" "Priority A items" tags-todo "+PRIORITY=\"A\"")
          ("pb" "Priority B items" tags-todo "+PRIORITY=\"B\"")
          ("pc" "Priority C items" tags-todo "+PRIORITY=\"C\"")
          ("pl" "Project List" ((tags "+PROJECT-TODO=\"DONE\"-TODO=\"ABANDONDED\"") (org-agenda-use-tag-inheritance nil)))
          ("r" "Weekly Review" agenda "" ((org-agenda-span 'week)
                                          (org-agenda-log-mode t)
                                          (org-agenda-log-mode-items '(closed clock state))
                                          (org-agenda-archives-mode t)))
          ("w" . "WORK searches")
          ("wa" "Today's agenda and WORK items" ((agenda "" ((org-agenda-span 'day)
                                                             (org-agenda-archives-mode t)
                                                             (org-agenda-log-mode t)
                                                             (org-agenda-log-mode-items '(closed clock state))))
                                                 (tags-todo "-TODO=\"BLOCKED\"-TODO=\"WAITING\"-SOMEDAY"))
                                                 ((org-agenda-tag-filter-preset '("+WORK"))))
          ("wb" "WORK Blocked and Waiting items" ((tags-todo "+WORK+TODO=\"BLOCKED\"|+WORK+TODO=\"WAITING\"|+WORK+TODO=\"PR\"")))
          ("wc" "Currently active non-repeating WORK items" tags-todo "+WORK-SOMEDAY-REPEATING")
          ("we" "Work agenda for exporting to html" ((agenda "" ((org-agenda-span 'day)
                                                                 (org-agenda-archives-mode t)
                                                                 (org-agenda-log-mode t)
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
  (setq org-capture-templates
        ;; Personal templates
        '(("b" "Buy Item" entry (file+headline "~/org/lists.org" "Shopping List")
           "* %?\nEntered %u\n")
          ("d" "Deadline Item" entry (file+headline "~/org/me.org" "Tasks")
           "* TODO [#C] %?\nDEADLINE: %^t")
          ("j" "Journal Entry" entry (file+datetree "~/personal/journal.org")
           "* %U\n%?")
          ("m" "Someday/Maybe Item" entry (file+headline "~/org/me.org" "Someday / Maybe")
           "* TODO [#C] %?\nEntered %u\n")
          ("s" "Scheduled Item" entry (file+headline "~/org/me.org" "Tasks")
           "* TODO [#C] %?\nSCHEDULED: %^t")
          ("t" "Standard Todo" entry (file+headline "~/org/me.org" "Tasks")
           "* TODO [#C] %?\nEntered %u\n")

          ;; Work.org templates
          ("n" "Work Note" entry (file+headline "~/org/work/work.org" "Work Notes")
           "* %?\nEntered %u\n")
          ("w" "Work Someday / Maybe Todo" entry (file+headline "~/org/work/work.org" "Someday / Maybe WORK")
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

  (setq org-src-fontify-natively t)
  (setq org-src-tab-acts-natively nil)

  ;; global Effort estimate values
  (setq org-global-properties (quote (("Effort_ALL" . "0:15 0:30 1:00 2:00 3:00 4:00 5:00 6:00 0:00")
                                      ("Depth_ALL" . "Light Deep"))))

  ;; If clocking out on a zero-time task, delete the clock entry entirely
  (setq org-clock-out-remove-zero-time-clocks t)

  ;; Persist the clock through emacs reboots
  (setq org-clock-persist t)

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

  ;; And don't preserve IDs whe cloning
  (setq org-clone-delete-id t)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Org-habit configuration
  ;; Always show all habits on the agenda, even if we don't need to do them today
  (setq org-habit-show-all-today t)

  ;; On multi-day agenda pages, show habits done/scheduled for all days
  (setq org-habit-show-habits-only-for-today nil)

  ;; Move the habits more to the right than the default (40)
  (setq org-habit-graph-coumn 120)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Keybindings for org-mode
  (global-set-key "\C-ca" 'org-agenda)
  (global-set-key "\C-cb" 'org-iswitchb)
  (global-set-key "\C-cc" 'org-capture)
  (global-set-key "\C-cl" 'org-store-link)

  (global-set-key (kbd "<f12>") 'brh/smart-agenda)
  (global-set-key (kbd "<f8>") 'org-agenda)
  (global-set-key (kbd "<f9>") 'org-clock-goto)

)
