; -*-Lisp-*-

(require 'package)

(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))

(setq package-enable-at-startup nil)
(package-initialize)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ido-enable-flex-matching t)
 '(ido-mode (quote both) nil (ido))
 '(org-agenda-files
   (quote
    ("~/org/logs.org" "~/org/lists.org" "~/org/work.org" "~/org/habits.org" "~/org/todo.org")))
 '(org-modules
   (quote
    (org-bbdb org-bibtex org-ctags org-docview org-gnus org-habit org-info org-irc org-mhe org-rmail org-w3m)))
 '(package-selected-packages
   (quote
    (spacemacs-theme projectile magit iedit evil-visual-mark-mode)))
 '(server-mode t))
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

; Make sure to have downloaded archive description.
(or (file-exists-p package-user-dir)
    (package-refresh-contents))

; Activate installed packages
(package-initialize)

; Install packages
(ensure-package-installed
              'goto-chg
	      'async
	      'helm
              'evil
              'iedit
              'flycheck
              'magit
              'org
              'projectile
              'spacemacs-theme
)

;;;;;;;;;;;;;;;;;;;;;;
;; My configuration ;;
;;
(setq inhibit-splash-screen t)
(setq vc-follow-symlinks t)

; Show line numbers
(global-linum-mode t)

; Cosmetics (color, toolbars)
(require 'spacemacs-dark-theme)
(toggle-scroll-bar -1)
(tool-bar-mode -1)

; When I'm ready ...
; (menu-bar-mode -1)

; Disable auto-saves
(setq auto-save-default nil)

; Disable backups
(setq make-backup-files nil)

;;;;;;;;;;;;;;;;;;;;;;;
;; Plugin Configuration
;;
(require 'helm-config)
(dired-async-mode 1)

; Allow asynchronous compilation of packages
(async-bytecomp-package-mode 1)

; Toggle evil mode with C-`
(setq evil-toggle-key "C-`")
(require 'evil)
(evil-mode t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org mode hotkey bindings
;;
(require 'org)

; Use ido-mode for auto-completion in org-mode. When I'm ready, consider Helm instead.
(setq org-completion-use-ido t)

; Default TODO progression sequence.
(setq org-use-fast-todo-selection t)
(setq org-todo-keywords '((sequence "TODO(t)" "BLOCKED(b)" "WIP(w)" "|" "DONE(d)")))

; Log completion time of DONE items
(setq org-log-done 'time)

; Default notes file for capture
(setq org-default-notes-file "~/org/todo.org")
(global-set-key "\C-cc" 'org-capture)

; Open up the agenda with C-c a
(global-set-key "\C-ca" 'org-agenda)

(global-set-key "\C-cb" 'org-iswitchb)
(global-set-key "\C-cl" 'org-store-link)

; Jump to the todo.org file
(global-set-key (kbd "C-c o")
		(lambda () (interactive) (find-file "~/org/todo.org")))

; Set org-refile to autocomplete three levels deep and check all agenda files
(setq org-refile-targets '((org-agenda-files . (:maxlevel . 3))))

; Org Agenda custom searches
(setq org-agenda-custom-commands
      '(("x" agenda)
        ("h" tags-todo "home")
        ("w" tags-todo "work")))
