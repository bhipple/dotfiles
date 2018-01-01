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
; (with-eval-after-load 'dired
;   (dired-async-mode 1))

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

;; salt-mode for jinja files
(add-to-list 'auto-mode-alist '("\\.jinja\\'" . salt-mode))

;; python-mode for waf scripts
(add-to-list 'auto-mode-alist '("\\wscript\\'" . python-mode))

;; Tramp with ssh
(setq tramp-default-method "ssh")

;; Yasnippet settings: just indent to starting column, instead of using emacs
;; auto-indent. Auto messes up yaml, python, salt files, etc.
(setq yas-indent-line 'fixed)

;; Use tab-n-go company
(with-eval-after-load 'company (company-tng-configure-default))

;; Tell Spacemacs to put org clocks on my modeline by default
(setq spaceline-org-clock-p t)

;; YCM Configuration
(setq ycmd-server-command (list "python" (file-truename "~/.nix-profile/bin/ycmd")))
(setq ycmd-force-semantic-completion nil)
(with-eval-after-load 'company global-ycmd-mode)

;; When in markdown mode, enable minor orgtbl-mode and format tables as markdown on save
(add-hook 'markdown-mode-hook 'orgtbl-mode)
(add-hook 'markdown-mode-hook
          (lambda ()
            (add-hook 'after-save-hook 'brh/cleanup-org-tables nil 'make-it-local)))
