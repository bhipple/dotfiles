;; -*-Lisp-*-

;; Setup http proxies, if necessary
(defun enable-bb-proxies (setq url-proxy-services '(("no_proxy" . "&\\(localhosti\\|127.0.0.1\\)")
                             ("http" . "http://proxy.inet.bloomberg.com:81")
                             ("https" . "http://proxy.inet.bloomberg.com:81")
                             )))

(defun disable-bb-proxies (makeunbound 'url-proxy-services))

(require 'package)

(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)

(setq package-enable-at-startup nil)
(package-initialize)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-files
   (quote
    ("~/org/habits.org" "~/org/accomplishments.org" "~/org/logs.org" "~/org/lists.org" "~/org/work.org" "~/org/todo.org")))
 '(org-modules
   (quote
    (org-bbdb org-bibtex org-ctags org-docview org-gnus org-habit org-info org-irc org-mhe org-rmail org-w3m)))
 '(package-selected-packages
   (quote
    (multi-term yaml-mode f flycheck-ycmd company-ycmd ycmd yasnippet company-quickhelp company helm-org-rifle helm-projectile helm-descbinds helm-dash intero org-jira highlight-chars helm flycheck evil-surround evil-numbers evil-magit evil-leader evil-exchange evil async spacemacs-theme projectile magit iedit evil-visual-mark-mode)))
 '(server-mode t)
 '(term-bind-key-alist
   (quote
    (("C-c C-c" . term-interrupt-subjob)
     ("C-c C-e" . term-send-esc)
     ("C-p" . previous-line)
     ("C-n" . next-line)
     ("C-s" . isearch-forward)
     ("C-r" . isearch-backward)
     ("C-m" . term-send-return)
     ("C-y" . term-paste)
     ("M-f" . term-send-forward-word)
     ("M-b" . term-send-backward-word)
     ("M-o" . term-send-backspace)
     ("M-p" . term-send-up)
     ("M-n" . term-send-down)
     ("M-M" . term-send-forward-kill-word)
     ("M-N" . term-send-backward-kill-word)
     ("<C-backspace>" . term-send-backward-kill-word)
     ("M-r" . term-send-reverse-search-history)
     ("M-d" . term-send-delete-word)
     ("M-," . term-send-raw)
     ("M-." . comint-dynamic-complete)
     ("C-w" . evil-window-next)
     ("C-h" . evil-window-left)
     ("C-j" . evil-window-down)
     ("C-k" . evil-window-up)
     ("C-l" . evil-window-right)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(when (not package-archive-contents)
    (package-refresh-contents))

;;;;;;;;;;;;;;;;;;;;;;;;
;; Package management ;;
;;;;;;;;;;;;;;;;;;;;;;;;
(defun ensure-package-installed (&rest packages)
  "Assure every package is installed, ask for installation if it’s not.

Return a list of installed packages or nil for every skipped package."
  (mapcar
   (lambda (package)
     (if (package-installed-p package)
         nil
       (if (y-or-n-p (format "Package %s is missing. Install it? " package))
           (package-install package)
         package)))
   packages))

;; Make sure to have downloaded archive description.
(or (file-exists-p package-user-dir)
    (package-refresh-contents))

;; Activate installed packages
(package-initialize)

;; Install packages
(ensure-package-installed
              'async
              'company
              'company-quickhelp
              'company-ycmd
              'evil
              'evil-exchange
              'evil-leader
              'evil-magit
              'evil-numbers
              'evil-surround
              'eww
              'f
              'flycheck
              'flycheck-ycmd
              'goto-chg
              'helm
              'helm-dash
              'helm-descbinds
              'helm-org-rifle
              'helm-projectile
              'highlight-chars
              'iedit
              'intero
              'magit
              'multi-term
              'org
              'org-jira
              'projectile
              's
              'spacemacs-theme
              'time
              'yaml-mode
              'yasnippet
              'ycmd
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; My configuration ;;
;;
(setq inhibit-splash-screen t)
(setq vc-follow-symlinks t)

(setq system-uses-terminfo nil)

;; Show line numbers
(global-linum-mode t)

;; Globally enable () matching as a minor mode
(electric-pair-mode)

;; Cosmetics (color, toolbars)
(require 'spacemacs-dark-theme)
(toggle-scroll-bar -1)
(tool-bar-mode -1)

;; When I'm ready ...
;; (menu-bar-mode -1)

;; Disable auto-saves
(setq auto-save-default nil)

;; Disable backups
(setq make-backup-files nil)

;; Fix tabs
(setq-default indent-tabs-mode nil)
(setq c-basic-indent 4)
(setq tab-width 4)

;; Highlighting whitespace
(require 'highlight-chars)
(hc-highlight-tabs)
;; TODO: Flashes while typing in insert mode. See how to get it to only show in normal mode.
;;(hc-highlight-trailing-whitespace)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Plugin Configuration
;;
(dired-async-mode 1)

;; Allow asynchronous compilation of packages
(async-bytecomp-package-mode 1)

;; Terminal Configuration
(require 'multi-term)
(setq multi-term-program "/home/bhipple/.nix-profile/bin/bash")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Company Configuration
;; Enable company mode everywhere
(add-hook 'after-init-hook 'global-company-mode)

(setq company-idle-delay 0.01)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Evil Configuration
;; These lines must come before (require 'evil)
(setq evil-want-C-u-scroll t)
(require 'evil-leader)
(global-evil-leader-mode)

;; Toggle evil mode with C-`
(setq evil-toggle-key "C-`")

(require 'evil)
(evil-mode t)

;; Leader keys
(evil-leader/set-leader "<SPC>")
(evil-leader/set-key
              "b" 'helm-projectile-switch-to-buffer
              "f" 'helm-find-files
              "gg" 'helm-projectile-grep
              "mf" 'magit-pull-from-upstream
              "mp" 'magit-push-current-to-upstream
              "ms" 'magit-status
              "ss" 'org-sort-entries
              "si" 'helm-dash-at-point
              "t" 'multi-term)

(require 'evil-magit)

(require 'evil-exchange)
;; Defaults to gx. cx might run into compatibility issues;
;; consider (evil-exchange-cx-install) if I'm feeling up for it.
(evil-exchange-install)

(require 'evil-surround)
(global-evil-surround-mode 1)

(require 'evil-numbers)
(global-set-key (kbd "C-c +") 'evil-numbers/inc-at-pt)
(global-set-key (kbd "C-c -") 'evil-numbers/dec-at-pt)

(global-set-key (kbd "C-h") 'evil-window-left)
(global-set-key (kbd "C-j") 'evil-window-down)
(global-set-key (kbd "C-k") 'evil-window-up)
(global-set-key (kbd "C-l") 'evil-window-right)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org mode hotkey bindings
;;
(require 'org)

;; Set indentation
(setq org-startup-indented t)

;; Default TODO progression sequence.
(setq org-use-fast-todo-selection t)
(setq org-todo-keywords '((sequence "TODO(t)" "BLOCKED(b)" "WIP(w)" "|" "DONE(d)")))

;; Log completion time of DONE items
(setq org-log-done 'time)

;; Tags
(setq org-tag-alist '(("ALGOS" . ?a)
                      ("CODING" . ?c)
                      ("DEEP" . ?d)
                      ("EMACS" . ?e)
                      ("HOME" . ?h)
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
      '(("b" "Buy Item" entry (file+headline "~/org/lists.org" "Shopping List")
             "* %?\nEntered %u\n")
        ("d" "Deadline item" entry (file+headline "~/org/work.org" "General Notes")
             "* TODO [#C] %?\nDEADLINE: %^t")
        ("s" "Scheduled item" entry (file+headline "~/org/todo.org" "Tasks")
             "* TODO [#C] %?\nSCHEDULED: %^t")
        ("t" "Standard Todo" entry (file+headline "~/org/todo.org" "Tasks")
             "* TODO [#C] %?\nEntered %u\n")
        ("n" "Work Note" entry (file+headline "~/org/work.org" "General Notes")
             "* %?\nEntered %u\n")))

;; Default notes file for capture
(setq org-default-notes-file "~/org/todo.org")
(global-set-key "\C-cc" 'org-capture)

;; Open up the agenda with C-c a
(global-set-key "\C-ca" 'org-agenda)

(global-set-key "\C-cb" 'org-iswitchb)
(global-set-key "\C-cl" 'org-store-link)

;; Jump to the todo.org file
(global-set-key (kbd "C-c o")
        (lambda () (interactive) (find-file "~/org/todo.org")))

;; Set org-refile to autocomplete three levels deep and check all agenda files
(setq org-refile-targets '((org-agenda-files . (:maxlevel . 3))))

;; Archive to subdirectory
(setq org-archive-location "~/org/archive/%s_archive::")

;; Org Agenda custom searches
(setq org-agenda-custom-commands
      '(("x" agenda)
        ("h" tags-todo "HOME")
        ("w" tags-todo "WORK")))

;; Org Babel language integration
(require 'ob-haskell)
(require 'ob-python)
(require 'ob-sh)

;; Org mode and Jira Integration
(setq jiralib-url "https://jira6.prod.bloomberg.com")

(setq request-log-level 'debug)
(setq request-message-level 'debug)

;;;;;;;;;;;;;;;;;;;;;
;; Helm Configuration
(require 'helm-config)
(helm-mode 1)

;; Use helm for M-x
(global-set-key (kbd "M-x") 'helm-M-x)

(require 'helm-org-rifle)

;; Provide the describe-bindings function, which shows all keyboard
;; shortcuts currently active in your major + minor modes
(require 'helm-descbinds)
(helm-descbinds-mode)

;; Use Projectile with Helm
(projectile-global-mode)
(setq projectile-completion-system 'helm)
(setq projectile-switch-project-action 'helm-projectile)

;; Enable caching. Invalidate the current project cache with C-c p i
(setq projectile-enable-caching t)

(helm-projectile-on)

;; List of times to show in helm-world-time
;; TODO: Not working due to issues with time.el?
(setq display-time-world-list '(("America/New_York" "New_York")
                                ("UTC" "UTC")
                                ("Europe/London" "London")
                                ("Europe/Amsterdam" "Amsterdam")
                                ("Asia/Shanghai" "Shanghai")
                                ("Asia/Tokyo" "Tokyo")
                                ("Australia/Sydney" "Sydney")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dash Documentation Config
(setq helm-dash-docsets-path (format "%s/.docsets" (getenv "HOME")))

;; TODO: Setup local buffer documentation switching.

;; Some docsets save themselves in different files than their docset name
(defun brh/dash-docset-path (docset)
  (concat helm-dash-docsets-path "/"
          (cond
             ((string= docset "Ruby_2") "Ruby")
             (t docset))
          ".docset"))

(defun brh/dash-install (docset)
  (unless (file-exists-p (brh/dash-docset-path docset))
    (helm-dash-install-docset docset)))

(brh/dash-install "Bash")
(brh/dash-install "C")
(brh/dash-install "C++")
(brh/dash-install "Chef")
(brh/dash-install "Emacs Lisp")
(brh/dash-install "Haskell")
(brh/dash-install "JavaScript")
(brh/dash-install "Markdown")
(brh/dash-install "Python 2")
(brh/dash-install "Ruby_2")
(brh/dash-install "CMake")
(brh/dash-install "Docker")
(brh/dash-install "Groovy")

(setq helm-dash-common-docsets
      '("C++"
        "Emacs Lisp"
        "Haskell"
        "Python 2"))

(mapcar (lambda (d) (brh/dash-install d)) helm-dash-common-docsets)

;;(require 'eww)
;;(setq helm-dash-browser-func 'eww)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Haskell Configuration
(defun haskell-doc ()
  (interactive)
  (setq-local helm-dash-docsets '("Haskell")))

(add-hook 'haskell-mode-hook 'intero-mode)
(add-hook 'haskell-mode-hook 'haskell-doc)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; YAML CONFIGURATION
(add-hook 'yaml-mode-hook
    '(lambda ()
    (define-key yaml-mode-map "\C-m" 'newline-and-indent)))
