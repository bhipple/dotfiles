;;; packages.el --- brh-general layer packages file for Spacemacs.
;;
(defconst brh-general-packages '(
  direnv
  gcmh
  jupyter
  nix-update
  popper
  s
  solarized-theme
))

(defun brh-general/init-direnv ()
  (use-package direnv
    :config (direnv-mode)))

(defun brh-general/init-nix-update ()
  (use-package nix-update))

(defun brh-general/init-popper ()
  (use-package popper
    :init
    (setq popper-reference-buffers
          '(
            ;help-mode
            ;compilation-mode
            "\\*Messages\\*"
            "Output\\*$"
            "\\*Async Shell Command\\*"
            ))
    (popper-mode +1)
    (popper-echo-mode +1)))

(defun brh-general/init-jupyter ()
  (use-package jupyter))

(defun brh-general/init-gcmh ()
  (use-package gcmh
    :init
    (gcmh-mode 1)))

(defun brh-general/init-s ()
  (use-package s))

(defun brh-general/init-solarized-theme ()
  (use-package solarized-theme))
