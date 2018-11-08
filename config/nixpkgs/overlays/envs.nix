self: super:
{
  spacemacs = super.emacsWithPackages (epkgs: (with epkgs.melpaStablePackages; [
    # archives
    # auto-highlight-symbol
    # centered-cursor-mode
    # chruby
    # clang-format
    # clean-aindent-mode
    # cmm-mode
    # column-enforce-mode
    # company-c-headers
    # company-lua
    # company-plsense
    # company-rtags
    # confluence
    # csv-mode
    # dactyl-mode
    # disaster
    # evil-cleverparens
    # evil-ediff
    # evil-exchange
    # evil-goggles
    # evil-indent-plus
    # evil-ledger
    # evil-lion
    # evil-unimpaired
    # flycheck-bashate
    # flycheck-rtags
    # font-lock
    # gh-md
    # gitignore-templates
    # go-gen-test
    # google-c-style
    # helm-css-scss
    # helm-flx
    # helm-gitignore
    # helm-hoogle
    # helm-rtags
    # helm-xref
    # highlight
    # hlint-refactor
    # insert-shebang
    # jenkins
    # js-doc
    # link-hint
    # livid-mode
    # lorem-ipsum
    # mmm-mode
    # mvn
    # nix-update
    # ob-restclient
    # open-junk-file
    # org-brain
    # org-plus-contrib
    # org-present
    # p4
    # password-generator
    # php-extras
    # pipenv
    # pippel
    # prettier-js
    # pytest
    # restclient
    # restclient-helm
    # rpm-spec-mode
    # ruby-refactor
    # seeing-is-believing
    # spinner
    # sql-indent
    # undo-tree
    # uuidgen
    # vi-tilde-fringe
    # vimrc-mode
    # zeal-at-point
    ac-ispell
    ac-php-core
    ace-jump-helm-line
    ace-link
    ace-window
    adoc-mode
    aggressive-indent
    alert
    all-the-icons
    anaconda-mode
    ansible
    ansible-doc
    anzu
    async
    auto-compile
    auto-complete
    auto-dictionary
    auto-yasnippet
    avy
    bind-key
    bind-map
    browse-at-remote
    bundler
    company
    company-anaconda
    company-ansible
    company-cabal
    company-emacs-eclim
    company-ghc
    company-go
    company-nixos-options
    company-php
    company-quickhelp
    company-restclient
    company-shell
    company-statistics
    company-tern
    company-terraform
    company-web
    company-ycmd
    concurrent
    counsel
    counsel-projectile
    ctable
    cython-mode
    dash
    dash-functional
    deferred
    define-word
    diff-hl
    diminish
    docker
    docker-tramp
    dockerfile-mode
    # doom-modeline
    dotenv-mode
    # drupal-mode
    dumb-jump
    eclim
    editorconfig
    eldoc-eval
    elisp-slime-nav
    emmet-mode
    engine-mode
    ensime
    epc
    epl
    esh-help
    eshell-prompt-extras
    eshell-z
    eval-sexp-fu
    evil
    evil-anzu
    evil-args
    evil-escape
    evil-iedit-state
    evil-lisp-state
    evil-magit
    evil-matchit
    evil-mc
    evil-nerd-commenter
    evil-numbers
    evil-org
    evil-surround
    evil-tutor
    evil-visual-mark-mode
    evil-visualstar
    expand-region
    eyebrowse
    f
    fancy-battery
    fill-column-indicator
    fish-mode
    flx
    flx-ido
    flycheck
    flycheck-haskell
    flycheck-ledger
    flycheck-pos-tip
    flycheck-ycmd
    flyspell-correct
    flyspell-correct-helm
    fringe-helper
    fuzzy
    ggtags
    ghc
    git-commit
    git-gutter
    git-gutter-fringe
    git-link
    git-messenger
    git-timemachine
    gitattributes-mode
    gitconfig-mode
    gitignore-mode
    gntp
    gnuplot
    go-eldoc
    go-fill-struct
    go-guru
    go-impl
    go-mode
    go-rename
    go-tag
    godoctor
    golden-ratio
    google-translate
    goto-chg
    gradle-mode
    graphviz-dot-mode
    groovy-imports
    groovy-mode
    haml-mode
    haskell-mode
    haskell-snippets
    hcl-mode
    helm
    helm-ag
    helm-c-yasnippet
    helm-company
    helm-core
    helm-dash
    helm-descbinds
    helm-git-grep
    helm-gtags
    helm-make
    helm-mode-manager
    helm-nixos-options
    helm-org-rifle
    helm-projectile
    helm-purpose
    helm-pydoc
    helm-swoop
    helm-themes
    hierarchy
    highlight-indentation
    highlight-numbers
    highlight-parentheses
    hindent
    hl-todo
    ht
    htmlize
    hungry-delete
    hydra
    ibuffer-projectile
    iedit
    imenu-list
    impatient-mode
    importmagic
    indent-guide
    inf-ruby
    ivy
    jinja2-mode
    js2-mode
    js2-refactor
    json-mode
    json-navigator
    json-reformat
    json-snatcher
    know-your-http-well
    ledger-mode
    live-py-mode
    log4e
    lua-mode
    macrostep
    magit
    magit-gitflow
    magit-popup
    magit-svn
    markdown-mode
    markdown-toc
    markup-faces
    maven-test-mode
    meghanada
    memoize
    minitest
    mmm-jinja2
    move-text
    multi-term
    multiple-cursors
    mwim
    nameless
    nginx-mode
    nix-mode
    nixos-options
    ob-http
    org-bullets
    org-category-capture
    org-download
    org-jira
    org-mime
    org-pomodoro
    org-projectile
    orgit
    overseer
    ox-gfm
    ox-pandoc
    ox-twbs
    package-lint
    packed
    pandoc-mode
    paradox
    paredit
    parent-mode
    pcache
    pcre2el
    persp-mode
    pfuture
    php-auto-yasnippets
    php-mode
    phpcbf
    phpunit
    pip-requirements
    pkg-info
    popup
    popwin
    pos-tip
    powerline
    projectile
    pug-mode
    py-isort
    pyenv-mode
    pythonic
    pyvenv
    rainbow-delimiters
    rake
    rbenv
    request
    request-deferred
    restart-emacs
    robe
    rspec-mode
    rtags
    rubocop
    ruby-hash-syntax
    ruby-test-mode
    ruby-tools
    rvm
    s
    salt-mode
    sass-mode
    sbt-mode
    scala-mode
    scss-mode
    shell-pop
    # shrink-path
    simple-httpd
    skewer-mode
    slim-mode
    smartparens
    smeargle
    spaceline
    spaceline-all-the-icons
    string-inflection
    swiper
    symon
    systemd
    tablist
    tagedit
    tern
    terraform-mode
    toc-org
    treemacs
    treemacs-evil
    treemacs-projectile
    unfill
    use-package
    volatile-highlights
    web-beautify
    web-completion-data
    web-mode
    which-key
    window-purpose
    winum
    with-editor
    ws-butler
    xcscope
    xml-rpc
    xterm-color
    yaml-mode
    yapfify
    yasnippet
    yasnippet-snippets
    ycmd
  ]));

  # Minimal set of packages to install everywhere
  minEnv = super.hiPrio (super.buildEnv {
    name = "minEnv";
    paths = [
      self.bashInteractive
      self.bat
      self.bc
      self.coreutils
      self.curl
      self.feh
      self.file
      self.gitAndTools.hub
      self.global
      self.gnused
      self.gnutar
      self.htop
      self.jq
      self.nox
      self.par
      self.pass
      self.pinentry
      self.procps
      self.ripgrep
      self.rlwrap
      self.tmux
      self.tree
      self.unzip
      self.wget
      self.ycmd
      self.zsh
    ];
  });

  # For "permanent" systems
  bigEnv = super.hiPrio (super.buildEnv {
    name = "bigEnv";
    paths = [
      self.alsaUtils
      self.aspell
      self.bind
      self.calibre
      self.chromium
      self.cmake
      self.digikam
      self.emacs
      self.firefox
      self.git-crypt
      self.gnumake
      self.gnupg
      self.gnutls
      self.graphviz
      self.httpie
      self.icu
      self.imagemagick
      self.irssi
      self.ledger
      self.lftp
      self.mupdf
      self.mupdf
      self.neovim
      self.nethogs
      self.nixops
      self.nodePackages.tern  # Needed by spacemacs JS layer
      self.pandoc
      self.pdsh
      self.poppler_utils
      self.shellcheck
      self.sloc
      self.source-code-pro
      self.stack
      self.truecrypt
      self.upower
      self.vagrant
      self.vimPlugins.youcompleteme
      self.vlc
      self.weechat
      self.xclip
      self.xsel
      self.zeal
      self.zlib
    ];
  });

  pyEnv = super.hiPrio (self.python3.withPackages (ps: with ps; [
    flake8
    isort
    numpy
    ofxclient
    paramiko
    pep8
    pylint
    setuptools
    toolz
    yamllint
    yapf
  ]));

  haskellEnv = self.haskellPackages.ghcWithPackages (ps: with ps; [
    Cabal
    async
    dhall
    dhall-bash
    dhall-json
    dhall-nix
    filepath
    ghc-mod
    hindent
    hlint
    hoogle
    optparse-generic
    text
    text-show
    trifecta
    turtle
    xmobar
    xmonad
    xmonad-contrib
    xmonad-extras
  ]);
}
