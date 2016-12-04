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
 '(package-selected-packages
   (quote
    (spacemacs-theme projectile magit iedit evil-visual-mark-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


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
              'evil
              'iedit
              'magit
              'org
              'projectile
              'spacemacs-theme
)

;;;;;;;;;;;;;;;;;;;;;;
;; My configuration ;;
;;;;;;;;;;;;;;;;;;;;;;
(setq inhibit-splash-screen t)

;; Show line numbers
(global-linum-mode t)

;; Cosmetics (color, toolbars)
(require 'spacemacs-dark-theme)
(toggle-scroll-bar -1)
(tool-bar-mode -1)

;; When I'm ready ...
;; (menu-bar-mode -1)

;; Disable auto-saves
(setq auto-save-default -1)

(require 'evil)
(evil-mode t)

;; Org mode hotkey bindings
(require 'org)

(setq org-todo-keywords '((sequence "TODO" "WIP" "DONE")))

;; Log completion time of DONE items
(setq org-log-done 'time)

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-iswitchb)