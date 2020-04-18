;; General configuration settings

;; Fix tabs
(setq-default indent-tabs-mode nil)
(setq c-basic-indent 4)
(setq tab-width 4)

;; Don't ask about following symlinks
(setq vc-follow-symlinks t)

;; Don't show gravitar images next to committers
(setq magit-revision-show-gravatars nil)

;; Don't depend on $TERM
(setq system-uses-terminfo nil)

;; Big performance boost
(setq auto-window-vscroll nil)

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

;; Tramp with ssh
(setq tramp-default-method "ssh")

;; Yasnippet settings: just indent to starting column, instead of using emacs
;; auto-indent. Auto messes up yaml, python, salt files, etc.
(setq yas-indent-line 'fixed)

;; Use tab-n-go company
(with-eval-after-load 'company (company-tng-configure-default))

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

; Fix for emacs 26.1 and ansi-term evil movement
; See https://github.com/syl20bnr/spacemacs/issues/10779
(setq term-char-mode-point-at-process-mark nil)

;; Disables smartparens while still keeping it around for Spacemacs to use
;; https://github.com/syl20bnr/spacemacs/issues/12533
(with-eval-after-load 'smartparens (progn
 (defadvice spacemacs-editing/init-smartparens (around disable-smartparens activate))
 (show-smartparens-global-mode -1)
 (show-smartparens-mode -1)
 (turn-off-smartparens-mode)
 (turn-off-smartparens-strict-mode)
 (remove-hook 'comint-mode-hook 'smartparens-mode)
 (remove-hook 'prog-mode-hook 'smartparens-mode)
 (remove-hook 'minibuffer-setup-hook 'spacemacs//conditionally-enable-smartparens-mode)))

; Work-around for issue with evil search and minibuffer causing d, y, etc. to be
; entered twice until an emacs restart. See here for details:
; https://github.com/syl20bnr/spacemacs/issues/10410#issuecomment-391641439
(defun kill-minibuffer ()
  (interactive)
  (when (windowp (active-minibuffer-window))
    (evil-ex-search-exit)))
(add-hook 'mouse-leave-buffer-hook #'kill-minibuffer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Performance / Garbage Collector Optimizations
;; Much of this logic is deferred to spacemancs and gcmh:
;; https://gitlab.com/koral/gcmh/-/tree/master
;; Show when the garbage collector runs
(setq gcmh-verbose t)

;; Set the max GC run at 2GB of RAM
(setq gcmh-high-cons-threshold #x80000000)

;; Per suggestion on lsp-mode, increase read-process output
;; https://github.com/emacs-lsp/lsp-mode/#performance
(setq read-process-output-max (* 4 1024 1024)) ;; 4mb

;; Disable savehist-mode to improve performance and potentially avoid crashes:
;; https://github.com/syl20bnr/spacemacs/issues/8462
(savehist-mode nil)

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
;; Dash Docset Configuration
; The docsets that I'm interested in. The full list of feed names is here:
; https://github.com/Kapeli/feeds
(setq brh/dash-docsets
      '("Bash"
        "Boost"
        "C++"
        "CMake"
        "Docker"
        "Emacs_Lisp"
        "Groovy"
        "Jinja"
        "NumPy"
        "Pandas"
        "Python3"
        "Rust"
        "SaltStack"))

; Browse right here in emacs!
(setq dash-docs-browser-func 'eww)

(add-hook 'c++-mode-hook
          (lambda () (setq-local dash-docs-docsets '("Boost" "C++" "CMake"))))

(add-hook 'dockerfile-mode-hook
          (lambda () (setq-local dash-docs-docsets '("Docker"))))

(add-hook 'emacs-lisp-mode-hook
          (lambda () (setq-local dash-docs-docsets '("Emacs Lisp"))))

(add-hook 'groovy-mode-hook
          (lambda () (setq-local dash-docs-docsets '("Groovy"))))

(add-hook 'python-mode-hook
          (lambda () (setq-local dash-docs-docsets '("Python 3" "Jinja" "NumPy" "Pandas" "SaltStack"))))

(add-hook 'rust-mode-hook
          (lambda () (setq-local dash-docs-docsets '("Rust"))))

(add-hook 'sh-mode-hook
          (lambda () (setq-local dash-docs-docsets '("Bash"))))
