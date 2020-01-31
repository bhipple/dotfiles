self: super: let

in {

  # Minimal set of packages to install everywhere
  minEnv = super.hiPrio (super.buildEnv {
    name = "minEnv";
    paths = [
      self.bashInteractive
      self.bat
      self.bc
      self.coreutils
      self.curl
      self.fd
      self.feh
      self.file
      self.fzf
      self.git-crypt
      self.gitAndTools.hub
      self.global
      self.gnused
      self.gnutar
      self.htop
      self.jq
      self.ledger
      self.nixpkgs-fmt
      self.nox
      self.par
      self.pass
      self.pinentry
      self.procps
      self.ripgrep
      self.rlwrap
      self.tmux
      self.tmuxPlugins.copycat
      self.tmuxPlugins.open
      self.tmuxPlugins.sensible
      self.tmuxPlugins.yank
      self.tree
      self.unzip
      self.uxterm
      self.wget
      self.zsh
    ];
  });

  # For "permanent" systems
  bigEnv = super.hiPrio (super.buildEnv {
    name = "bigEnv";
    paths = [
      self.alsaUtils
      self.aspell
      self.autoflake
      self.bind
      self.calibre
      self.chromium
      self.cmake
      self.dasht
      self.direnv
      self.firefox
      self.gnumake
      self.gnupg
      self.gnutls
      self.graphviz
      self.icu
      self.imagemagick
      self.irssi
      self.lftp
      self.mupdf
      self.nethogs
      self.nixops
      self.pandoc
      self.pdsh
      self.shellcheck
      self.sloc
      self.source-code-pro
      self.thunderbolt
      self.truecrypt
      self.upower
      self.vagrant
      self.vimPlugins.youcompleteme
      self.vim_configurable
      self.vlc
      self.xclip
      self.xsel
      self.youtube-dl
      self.zeal
      self.zlib
    ];
  });

  pyEnv = super.lowPrio (self.python3.withPackages (ps: with ps; [
    isort
    pep8
    pyflakes
    pyls-isort
    pytest
    pytest-mock
    python-language-server
    setuptools
    yamllint
    yapf
  ]));

  # Use nix-shell -p plaidPy for ledger cred updates
  plaidPy = super.lowPrio (self.python3.withPackages (ps: with ps; [
    plaid-python
    flask
  ]));
}
