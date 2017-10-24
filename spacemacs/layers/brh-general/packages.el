;;; packages.el --- brh-general layer packages file for Spacemacs.
;;
(defconst brh-general-packages
  '(groovy-mode
    jenkins
    org-jira
    rpm-spec-mode
    ))

(defun brh-general/init-groovy-mode ()
  (use-package groovy-mode))

(defun brh-general/init-jenkins ()
  (use-package jenkins))

(defun brh-general/init-org-jira ()
  (use-package org-jira))

(defun brh-general/init-rpm-spec-mode ()
  (use-package rpm-spec-mode))
