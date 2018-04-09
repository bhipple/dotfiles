self: super:
{
  # Minimal set of packages to install everywhere
  minEnv = super.hiPrio (super.buildEnv {
    name = "minEnv";
    paths = [
      self.ag
      self.bashInteractive
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
      self.nix-repl
      self.nox
      self.par
      self.pass
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

  # For "permanent" systems; compatible on both Mac and Linux
  bigEnv = super.hiPrio (super.buildEnv {
    name = "bigEnv";
    paths = [
      self.aspell
      self.bind
      self.chromium
      self.cmake
      self.emacs
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
      self.neovim
      self.nixops
      self.nodePackages.tern  # Needed by spacemacs JS layer
      self.pandoc
      self.pdsh
      self.shellcheck
      self.sloc
      self.source-code-pro
      self.stack
      self.truecrypt
      self.upower
      self.vagrant
      self.vimPlugins.youcompleteme
      self.weechat
      self.xclip
      self.xsel
      self.zeal
      self.zlib
    ];
  });

  # For "permanent" systems; these packages don't seem to play well with MacOS
  bigEnvLinux = super.hiPrio (super.buildEnv {
    name = "bigEnvLinux";
    paths = [
      self.calibre
      self.digikam
      self.haskellPackages.threadscope
      self.lftp
      self.mupdf
      self.vlc
    ];
  });

  pyEnv = super.hiPrio (self.python27.withPackages (ps: with ps; [
    flake8
    futures
    isort
    numpy
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
