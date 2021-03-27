;;; packages.el --- brh-general layer packages file for Spacemacs.
;;
(defconst brh-general-packages '(
  direnv
  (dumb-jump
   :location (recipe :fetcher github
                     :repo "bhipple/dumb-jump"
                     :branch "helm-bytecode-fix"))
  gcmh
  jupyter
  nix-update
  s
  solarized-theme
))

(defun brh-general/init-direnv ()
  (use-package direnv
    :config (direnv-mode)))

(defun brh-general/init-dumb-jump ()
  (use-package dumb-jump
    :init (setq dumb-jump-selector 'helm)))

(defun brh-general/init-nix-update ()
  (use-package nix-update))

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
