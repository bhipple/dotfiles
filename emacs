;; -*-Lisp-*-

;; Setup http proxies, if necessary
(defun brh/enable-bb-proxies ()
    (interactive)
    (setq url-proxy-services '(("no_proxy" . "&\\(localhosti\\|127.0.0.1\\)")
                             ("http" . "http://proxy.inet.bloomberg.com:81")
                             ("https" . "http://proxy.inet.bloomberg.com:81")
                             )))

(defun brh/disable-bb-proxies ()
  (interactive)
  (makeunbound 'url-proxy-services))

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
    ("~/org/habits.org" "~/org/logs.org" "~/org/lists.org" "~/org/work.org" "~/org/me.org")))
 '(org-modules
   (quote
    (org-bbdb org-bibtex org-ctags org-docview org-gnus org-habit org-info org-irc org-mhe org-rmail org-w3m)))
 '(package-selected-packages
   (quote
    (hindent use-package magithub nix-mode exec-path-from-shell multi-term yaml-mode f flycheck-ycmd company-ycmd ycmd yasnippet company-quickhelp company helm-org-rifle helm-projectile helm-descbinds helm-dash intero org-jira highlight-chars helm flycheck evil-surround evil-numbers evil-leader evil-exchange evil async spacemacs-theme projectile magit iedit evil-visual-mark-mode)))
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
       (progn
         (format "Package %s is missing. Install it? " package)
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
              'evil-numbers
              'evil-surround
              'eww
              'exec-path-from-shell
              'f
              'flycheck
              'flycheck-haskell
              'flycheck-pos-tip
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
              'magithub
              'multi-term
              'nix-mode
              'org
              'org-jira
              'pos-tip
              'projectile
              's
              'spacemacs-theme
              'time
              'yaml-mode
              'yasnippet
              'ycmd
              'use-package
)

(setq use-package-always-ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; My configuration ;;
;;
;; Fix for MacOS environment variables
;;(when (memq window-system '(mac ns))
  ;;(exec-path-from-shell-initialize))

(setq inhibit-splash-screen t)
(setq vc-follow-symlinks t)

(setq system-uses-terminfo nil)

;; Show line numbers
(global-linum-mode t)

;; Show column number in status bar
(setq column-number-mode t)

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

;; Flycheck
(require 'flycheck)
(global-flycheck-mode)

(require 'flycheck-pos-tip)
(flycheck-pos-tip-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Company Configuration

;; Enable company mode everywhere
(add-hook 'after-init-hook 'global-company-mode)

(setq company-idle-delay 0.01)
(setq company-selection-wrap-around t)

(require 'yasnippet)
(require 'company)

;; Add yasnippet support for all company backends
(defvar company-mode/enable-yas t
  "Enable yasnippet for all backends.")

(defun company-mode/backend-with-yas (backend)
  (if (or (not company-mode/enable-yas) (and (listp backend) (member 'company-yasnippet backend)))
      backend
    (append (if (consp backend) backend (list backend))
            '(:with company-yasnippet))))

(setq company-backends (mapcar #'company-mode/backend-with-yas company-backends))

;; Functions for handling company expansion with ya-snippets
(defun check-expansion ()
  (save-excursion
    (if (looking-at "\\_>") t
      (backward-char 1)
      (if (looking-at "\\.") t
    (backward-char 1)
    (if (looking-at "->") t nil)))))

(defun do-yas-expand ()
  (let ((yas-fallback-behavior 'return-nil))
    (yas-expand)))


(defun tab-indent-or-complete ()
  (interactive)
  (cond
   ((minibufferp)
    (minibuffer-complete))
   (t
    (indent-for-tab-command)
    (if (or (not yas-minor-mode)
        (null (do-yas-expand)))
    (if (check-expansion)
        (progn
          (company-manual-begin)
          (if (null company-candidates)
          (progn
            (company-abort)
            (indent-for-tab-command)))))))))

(defun tab-complete-or-next-field ()
  (interactive)
  (if (or (not yas-minor-mode)
      (null (do-yas-expand)))
      (if company-candidates
      (company-complete-selection)
    (if (check-expansion)
      (progn
        (company-manual-begin)
        (if (null company-candidates)
        (progn
          (company-abort)
          (yas-next-field))))
      (yas-next-field)))))

(defun expand-snippet-or-complete-selection ()
  (interactive)
  (if (or (not yas-minor-mode)
          (null (do-yas-expand))
          (company-abort))
      (company-complete-selection)))

(defun abort-company-or-yas ()
  (interactive)
  (if (null company-candidates)
      (yas-abort-snippet)
    (company-abort)))


(global-set-key [tab] 'tab-indent-or-complete)
(global-set-key (kbd "TAB") 'tab-indent-or-complete)
(global-set-key [(control return)] 'company-complete-common)

(define-key company-active-map [tab] 'expand-snippet-or-complete-selection)
(define-key company-active-map (kbd "TAB") 'expand-snippet-or-complete-selection)

(define-key yas-minor-mode-map [tab] nil)
(define-key yas-minor-mode-map (kbd "TAB") nil)

(define-key yas-keymap [tab] 'tab-complete-or-next-field)
(define-key yas-keymap (kbd "TAB") 'tab-complete-or-next-field)
(define-key yas-keymap [(control tab)] 'yas-next-field)
(define-key yas-keymap (kbd "C-g") 'abort-company-or-yas)

;(eval-after-load 'company '(progn
    ;(define-key company-active-map [tab] 'company-complete-common-or-cycle)
    ;(define-key company-active-map (kbd "TAB") 'company-complete-common-or-cycle)))

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
              "b" 'helm-buffers-list
              "df" 'magit-diff
              "f" 'helm-find-files
              "gg" 'helm-projectile-grep
              "mf" 'magit-pull-from-upstream
              "mp" 'magit-push-current-to-upstream
              "ms" 'magit-status
              "p" 'helm-projectile
              "ss" 'org-sort-entries
              "si" 'helm-dash-at-point
              "t" 'multi-term)

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
;; Magit Configuration
(use-package magit
  :bind (([tab] . magit-section-toggle)
         ("TAB" . magit-section-toggle)))

(use-package magithub
  :after magit
  :config (magithub-feature-autoinject t))

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
      '(("b" "Buy Item" entry (file+headline "~/org/lists.org" "Shopping List")
             "* %?\nEntered %u\n")
        ("d" "Deadline item" entry (file+headline "~/org/work.org" "General Notes")
             "* TODO [#C] %?\nDEADLINE: %^t")
        ("s" "Scheduled item" entry (file+headline "~/org/me.org" "Tasks")
             "* TODO [#C] %?\nSCHEDULED: %^t")
        ("t" "Standard Todo" entry (file+headline "~/org/me.org" "Tasks")
             "* TODO [#C] %?\nEntered %u\n")
        ("n" "Work Note" entry (file+headline "~/org/work.org" "General Notes")
             "* %?\nEntered %u\n")))

;; Default notes file for capture
(setq org-default-notes-file "~/org/me.org")
(global-set-key "\C-cc" 'org-capture)

;; Open up the agenda with C-c a
(global-set-key "\C-ca" 'org-agenda)

(global-set-key "\C-cb" 'org-iswitchb)
(global-set-key "\C-cl" 'org-store-link)

;; Jump to the me.org file
(global-set-key (kbd "C-c o")
        (lambda () (interactive) (find-file "~/org/me.org")))

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

;; Highlight source code blocks
(setq org-src-fontify-natively t)

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

(defun brh/dash-install-all ()
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
    (brh/dash-install "Groovy"))

(setq helm-dash-common-docsets
      '("C++"
        "Emacs Lisp"
        "Haskell"
        "Python 2"))

(mapcar (lambda (d) (brh/dash-install d)) helm-dash-common-docsets)

;;(require 'eww)
;;(setq helm-dash-browser-func 'eww)

;; Ya-Snippets
(require 'yasnippet)
(yas-global-mode 1)

(setq yas-snippet-dirs '("~/dotfiles/yasnippet-snippets"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Haskell Configuration
(defun haskell-doc ()
  (interactive)
  (setq-local helm-dash-docsets '("Haskell")))

(require 'intero)
(flycheck-add-next-checker 'intero '(warning . haskell-hlint))

;(use-package hindent
;  :defer t
;  :init
;  (when (locate-library "hindent")
;    (add-hook 'haskell-mode-hook #'hindent-mode)))
;
;(use-package company-ghci
;  :after haskell
;  :config
;  (push '(company-ghci :with company-yasnippet :with company-dabbrev) company-backends))

(add-hook 'haskell-mode-hook 'intero-mode)
(add-hook 'haskell-mode-hook 'haskell-doc)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; YAML CONFIGURATION
(add-hook 'yaml-mode-hook
    '(lambda ()
    (define-key yaml-mode-map "\C-m" 'newline-and-indent)))
