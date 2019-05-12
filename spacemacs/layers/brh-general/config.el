;; General configuration settings

;; Fix tabs
(setq-default indent-tabs-mode nil)
(setq c-basic-indent 4)
(setq tab-width 4)

;; Don't ask about following symlinks
(setq vc-follow-symlinks t)

;; Don't depend on $TERM
(setq system-uses-terminfo nil)

;; Big performance boost
(setq auto-window-vscroll nil)

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

; Shut up about line length, unless it's really bad
; TODO: Consider upstreaming into spacemacs' python-fill-column variable so this happens consistently and automatically.
(setq flycheck-flake8-maximum-line-length 120)

;; Strongly prefer splitting to the right in split-window-sensibly
(setq split-height-threshold nil)

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

;; YCMD Configuration
; TODO: Figure out how this thing is supposed to startup. Looks like it passes an --options_file on its own:
; https://github.com/abingham/emacs-ycmd/blob/master/ycmd.el#L2203
(setq ycmd-server-command '("/home/bhipple/.nix-profile/bin/ycmd"))

(set-variable 'ycmd-global-config "/home/bhipple/.ycm_extra_conf.py")

(setq ycmd-force-semantic-completion nil)
(with-eval-after-load 'company (global-ycmd-mode t))

;; Use the primary system clipboard when yanking
(setq x-select-enable-primary t)

;; When compiling, jump the buffer automatically on failures, and use a high -j
(setq compilation-auto-jump-to-first-error t)
(setq helm-make-arguments "-j48")
(setq helm-make-do-save t)

;; TODO: Clean this up and organize a bit better. I think I just need the setq stuff.
(setq evil-escape-excluded-states '(normal visual multiedit emacs motion)
      evil-escape-excluded-major-modes '(neotree-mode)
      evil-escape-key-sequence "jk"
      evil-escape-unordered-key-sequence t
      evil-escape-delay 0.25)

;; Disable savehist-mode to improve performance and potentially avoid crashes:
;; https://github.com/syl20bnr/spacemacs/issues/8462
(savehist-mode nil)

;; Disables smartparens while still keeping it around for Spacemacs to use
;; Also does not disable it for {}
;; TODO: This doesn't work and causes initialization errors?
;; (eval-after-load 'smartparens
;;   '(progn
;;      (sp-pair "(" nil :actions :rem)
;;      (sp-pair "[" nil :actions :rem)
;;      (sp-pair "'" nil :actions :rem)
;;      (sp-pair "\"" nil :actions :rem)))
