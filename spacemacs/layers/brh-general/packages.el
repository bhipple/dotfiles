;;; packages.el --- brh-general layer packages file for Spacemacs.
;;
(defconst brh-general-packages '(nix-update))

(defun brh-general/init-nix-update ()
  (use-package nix-update))
