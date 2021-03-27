;;; packages.el --- brh-org layer packages file for Spacemacs.
;; See the Spacemacs documentation and FAQs for instructions on how to implement
;; a new layer:
;;
;;   SPC h SPC layers RET
;;
;;
;; Briefly, each package to be installed or configured by this layer should be
;; added to `brh-org-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `brh-org/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `brh-org/pre-init-PACKAGE' and/or
;;   `brh-org/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst brh-org-packages '(org org-bookmark-heading org-mru-clock org-super-agenda)
  "The list of Lisp packages required by the brh-org layer.")

(defun brh-org/init-org-bookmark-heading ()
  (use-package org-bookmark-heading))

(defun brh-org/init-org-mru-clock ()
  (use-package org-mru-clock
    :init
    (setq org-mru-clock-completing-read #'helm-comp-read
          org-mru-clock-files #'org-agenda-files
          org-mru-clock-how-many 100
          org-mru-clock-predicate #'org-mru-clock-exclude-done-and-archived)))

(defun brh-org/init-org-super-agenda ()
  (use-package org-super-agenda))

(defun brh-org/post-init-org ()
  (with-eval-after-load 'org
    (require 'org-bookmark-heading)
    (require 'org-super-agenda)

    ; N.B. This ships with org, but still needs to be required to initialize
    (require 'org-habit)

    ; Don't enable this by default, because it looks terrible on weekly views; consider adding a keybinding to toggle
    ; (org-super-agenda-mode t)

    ;; Enable org-babel
    (org-babel-do-load-languages 'org-babel-load-languages '(
      (ditaa . t)
      (emacs-lisp . t)
      (gnuplot . t)
      (org . t)
      (python . t)
      (shell . t)
      ; N.B. Jupyter must be last in this list! It depends on others.
      (jupyter . t)
    ))

    ;; Don't ask for confirmation every time I hit C-c c
    (setq org-confirm-babel-evaluate nil)

    ;; Don't indent src code blocks
    (setq org-edit-src-content-indentation 0)

    ;; Setup persistence hooks for the clock
    (org-clock-persistence-insinuate)

    ;; Show the org clock in the mode line
    (spaceline-toggle-org-clock-on)

    ;; TODO: Figure out why this isn't working.
    (spaceline-define-segment brh/inactive-org "Segment to show on mode line when not clocking"
                              (if (and (fboundp 'org-clocking-p (org-clocking-p))) nil "NOT CLOCKING"))
    (spaceline-toggle-brh/inactive-org-on)

  ))
