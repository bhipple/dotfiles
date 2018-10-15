self: super:
{
  # TODO: Condense these into one env with separate lists, instead of multiple.

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
