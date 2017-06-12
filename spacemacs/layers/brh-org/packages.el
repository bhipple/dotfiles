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

(defconst brh-org-packages '(org)
  "The list of Lisp packages required by the brh-org layer.")

(defun brh-org/init-org-habit ()
  (use-package org-habit))

(defun brh-org/post-init-org ()
  (with-eval-after-load 'org
    (require 'org-habit)

    ;; Enable org-babel
    (org-babel-do-load-languages 'org-babel-load-languages '((shell . t)
                                                             (python . t)
                                                             (emacs-lisp . t)
                                                             (haskell . t)
                                                             ))

    ;; Setup persistence hooks for the clock
    (org-clock-persistence-insinuate)
  )
)
