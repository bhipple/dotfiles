;; General configuration settings

;; Fix tabs
(setq-default indent-tabs-mode nil)
(setq c-basic-indent 4)
(setq tab-width 4)

;; Don't ask about following symlinks
(setq vc-follow-symlinks t)

;; Don't depend on $TERM
(setq system-uses-terminfo nil)

;; Perform dired actions asynchronously
(with-eval-after-load 'dired
  (dired-async-mode 1))

;; Globally enable () matching as a minor mode
(electric-pair-mode)

;; Enable caching. Invalidate the current project cache with C-c p i
(setq projectile-enable-caching t)

;; Regex for buffers to automatically update when the file changes on disk
;; Matches temp files created by ediff like foofile.~[git ref]~, where
;; git ref could be a sha, remote_branch, or ref~N
(setq revert-without-query '(".*\.~[a-z0-9_]+~$"))

;; Make ediff do a split for side-by-side diffing
(setq ediff-split-window-function 'split-window-horizontally)

;; Open jinja files in salt-mode
(add-to-list 'auto-mode-alist '("\\.jinja\\'" . salt-mode))
