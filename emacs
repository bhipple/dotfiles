;; This file contains the remnants that I haven't ported over to spacemacs.

;;;;;;;;;;;;;;;;;;;;;;;;
;; Package management ;;
;;;;;;;;;;;;;;;;;;;;;;;;
;; Install packages
(ensure-package-installed
              'company-ycmd
              'f
              'flycheck
              'flycheck-haskell
              'flycheck-pos-tip
              'flycheck-ycmd
              'goto-chg
              'highlight-chars
              'iedit
              'intero
              'org-jira
              'pos-tip
              'projectile
              's
              'time
              'yaml-mode
              'yasnippet
              'ycmd
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Plugin Configuration
;;
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Evil Configuration
(require 'evil-exchange)
;; Defaults to gx. cx might run into compatibility issues;
;; consider (evil-exchange-cx-install) if I'm feeling up for it.
(evil-exchange-install)

(require 'evil-numbers)
(global-set-key (kbd "C-c +") 'evil-numbers/inc-at-pt)
(global-set-key (kbd "C-c -") 'evil-numbers/dec-at-pt)

(defun halve-other-window-height ()
  "Expand current window to use half of the other window's lines."
  (interactive)
  (enlarge-window (/ (window-height (next-window)) 2)))

;; (require 'evil-tabs)
;; (global-evil-tabs-mode t)

(defun brh/tabhandler (f)
  (funcall f)
  (helm-projectile))

(defun brh/tabedit ()
  "Create a new tab and run helm-projectile"
  (interactive)
  (brh/tabhandler 'elscreen-clone))

(defun brh/tabvsplit ()
  "Create a vertical split and run helm-projectile in it"
  (interactive)
  (brh/tabhandler 'split-window-right))

(defun brh/tabsplit ()
  "Create a horizontal split and run helm-projectile in it"
  (interactive)
  (brh/tabhandler 'split-window-below))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;
;; Helm Configuration
(require 'helm-config)
(helm-mode 1)

(require 'helm-org-rifle)

;; Enable caching. Invalidate the current project cache with C-c p i
(setq projectile-enable-caching t)

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
    (interactive)
    (brh/dash-install "Bash")
    (brh/dash-install "C")
    (brh/dash-install "C++")
    (brh/dash-install "CMake")
    (brh/dash-install "Chef")
    (brh/dash-install "Docker")
    (brh/dash-install "Emacs Lisp")
    (brh/dash-install "Groovy")
    (brh/dash-install "Haskell")
    (brh/dash-install "JavaScript")
    (brh/dash-install "Markdown")
    (brh/dash-install "Python 2")
    (brh/dash-install "Ruby_2"))

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
