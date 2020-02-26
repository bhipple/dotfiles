;;; packages.el --- brh-general layer packages file for Spacemacs.
;;
(defconst brh-general-packages '(direnv gcmh nix-update s solarized-theme tmux-pane))

(defun brh-general/init-direnv ()
  (use-package direnv
    :config (direnv-mode)))

(defun brh-general/init-nix-update ()
  (use-package nix-update))

(defun brh-general/init-gcmh ()
  (use-package gcmh
    :init
    (gcmh-mode 1)))

(defun brh-general/init-s ()
  (use-package s))

(defun brh-general/init-solarized-theme ()
  (use-package solarized-theme))

(defun brh-general/init-tmux-pane ()
  (use-package tmux-pane))

;; (defun brh-general/init-xclip ()
;;   (use-package xclip
;;    :init
;;    (when (not (display-graphic-p)) (xclip-mode))))
