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

(defconst brh-org-packages '(org orb-habit org-super-agenda)
  "The list of Lisp packages required by the brh-org layer.")

(defun brh-org/init-org-habit ()
  (use-package org-habit))

(defun brh-org/init-org-super-agenda ()
  (use-package org-super-agenda))

(defun brh-org/post-init-org ()
  (with-eval-after-load 'org
    (require 'org-habit)
    (require 'org-super-agenda)

    ; Don't enable this by default, because it looks terrible on weekly views; consider adding a keybinding to toggle
    ; (org-super-agenda-mode t)

    ;; Enable org-babel
    (org-babel-do-load-languages 'org-babel-load-languages '(
      (ditaa . t)
      (emacs-lisp . t)
      (gnuplot . t)
      (haskell . nil)
      (org . t)
      (perl . t)
      (python . t)
      (shell . t)
    ))

    ;; Setup persistence hooks for the clock
    (org-clock-persistence-insinuate)

    ;; Show the org clock in the mode line
    (spaceline-toggle-org-clock-on)

    ;; TODO: Figure out why this isn't working.
    (spaceline-define-segment brh/inactive-org "Segment to show on mode line when not clocking"
                              (if (and (fboundp 'org-clocking-p (org-clocking-p))) nil "NOT CLOCKING"))
    (spaceline-toggle-brh/inactive-org-on)

  ))
