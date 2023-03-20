;; General configuration settings

;; Very useful when debugging; incredibly annoying when not
(setq debug-on-error nil)

;; Don't popup the *Warnings* buffer unless there's an error (still logs)
(setq warning-minimum-level :error)

;; Don't show native-comp warnings
(setq native-comp-async-report-warnings-errors nil)

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

;; Use polmode in ein
(setq ein:polymode t)

;; Tramp with ssh
(setq tramp-default-method "ssh")

;; Yasnippet settings: just indent to starting column, instead of using emacs
;; auto-indent. Auto messes up yaml, python, salt files, etc.
(setq yas-indent-line 'fixed)

;; Tell Spacemacs to put org clocks on my modeline by default
(setq spaceline-org-clock-p t)

;; Use the primary system clipboard when yanking
(setq select-enable-clipboard t)
(setq select-enable-primary t)

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

; Work-around for issue with evil search and minibuffer causing d, y, etc. to be
; entered twice until an emacs restart. See here for details:
; https://github.com/syl20bnr/spacemacs/issues/10410#issuecomment-391641439
(defun kill-minibuffer ()
  (interactive)
  (when (windowp (active-minibuffer-window))
    (evil-ex-search-exit)))
(add-hook 'mouse-leave-buffer-hook #'kill-minibuffer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; UTF-8 everywhere, as detailed in
;; https://www.masteringemacs.org/article/working-coding-systems-unicode-emacs
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;; Treat clipboard input as UTF-8 string first; compound text next, etc.
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

;; These two are also necessary to avoid constant spam!
(set-file-name-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LSP Settings
(with-eval-after-load 'lsp-mode
  ; Get rid of file watchers entirely
  (setq lsp-enable-file-watchers nil)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Performance / Garbage Collector Optimizations
;; Much of this logic is deferred to spacemancs and gcmh:
;; https://gitlab.com/koral/gcmh/-/tree/master
;; Show when the garbage collector runs
(setq gcmh-verbose t)

;; Set the max GC run at 500 MB of RAM
(setq gcmh-high-cons-threshold #x20000000)

;; Disable savehist-mode to improve performance and potentially avoid crashes:
;; https://github.com/syl20bnr/spacemacs/issues/8462
(savehist-mode nil)

;; Disable auto saves and backups. These can cause lag *and* kick the GC
(setq dotspacemacs-auto-save-file-location nil)
(setq backup-inhibited t)
(setq auto-save-default nil)

;; Big performance boost
(setq auto-window-vscroll nil)

;; Another big performance boost
(spacemacs/disable-smooth-scrolling)

;; Disable extremely slow functions
(with-eval-after-load 'magit
  ; Don't annotate status pages with git tag info
  (defun magit-insert-tags-header ())
  ; Don't attempt to lookup gravitar images next to committers
  (setq magit-revision-show-gravatars nil)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Elfeed Configuration
(setq rmh-elfeed-org-files '("~/org/personal/elfeed.org"))

; Set elfeed dates to be bold, purple, and underlined
(custom-set-faces
 '(elfeed-search-date-face
   ((t :foreground "#f0f"
       :underline t))))

(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "chromium-browser")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Timers
; 5 minutes after startup, run org-roam-db-sync; then re-run it every 60m
(run-with-timer (* 5 60) (* 60 60) 'org-roam-db-sync)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Bugs / Workarounds that can hopefully be deleted someday

; Workaround for:
; https://github.com/nonsequitur/git-gutter-plus/issues/42
; https://github.com/syl20bnr/spacemacs/issues/12860
(with-eval-after-load 'git-gutter+
  (defun git-gutter+-remote-default-directory (dir file)
    (let* ((vec (tramp-dissect-file-name file))
           (method (tramp-file-name-method vec))
           (user (tramp-file-name-user vec))
           (domain (tramp-file-name-domain vec))
           (host (tramp-file-name-host vec))
           (port (tramp-file-name-port vec)))
      (tramp-make-tramp-file-name method user domain host port dir)))

  (defun git-gutter+-remote-file-path (dir file)
    (let ((file (tramp-file-name-localname (tramp-dissect-file-name file))))
      (replace-regexp-in-string (concat "\\`" dir) "" file))))
