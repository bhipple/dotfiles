;; Org variable configuration

;; Dynamically find all agenda files by walking the directory
(setq org-agenda-files (append (brh/find-org-file-recursively "~/org/" "org")))

;; Enable keybindings defined below for TODO selection
(setq org-use-fast-todo-selection t)

;; Default TODO progression sequence.
(setq org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "BLOCKED(b)" "WAITING(w)" "|" "DONE(d)")))

;; Log completion time of DONE items
(setq org-log-done 'time)

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
        ("p" "Programming Item" entry (file+headline "~/org/me.org" "Programming")
            "* TODO [#C] %?\n")
        ("s" "Scheduled Item" entry (file+headline "~/org/me.org" "Tasks")
            "* TODO [#C] %?\nSCHEDULED: %^t")
        ("t" "Standard Todo" entry (file+headline "~/org/me.org" "Tasks")
            "* TODO [#C] %?\n")

        ;; Work.org templates
        ("n" "Work Note" entry (file+headline "~/org/work/work.org" "Work Notes")
            "* %?\n")
        ("w" "Work Todo" entry (file+headline "~/org/work/work.org" "Work Tasks")
              "* TODO [#C] %?\n")))

;; Default notes file for capture
(setq org-default-notes-file "~/org/me.org")
(global-set-key "\C-cc" 'org-capture)

;; Open up the agenda with C-c a
(global-set-key "\C-ca" 'org-agenda)

(global-set-key "\C-cb" 'org-iswitchb)
(global-set-key "\C-cl" 'org-store-link)

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
        ("n" "Today's agenda and all TODOs" ((agenda "" ((org-agenda-span 'day)
                                                          (org-agenda-log-mode t)))
                                              (todo "TODO")))
        ("p" . "Priority searches")
        ("pa" "Priority A items" tags-todo "+PRIORITY=\"A\"")
        ("pb" "Priority B items" tags-todo "+PRIORITY=\"B\"")
        ("pc" "Priority C items" tags-todo "+PRIORITY=\"C\"")
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
        ("ws" "Search WORK items" ((tags-todo "+WORK") (search "")))
        ("ww" "All WORK items" tags-todo "WORK")))

;; Highlight source code blocks
(setq org-src-fontify-natively t)
