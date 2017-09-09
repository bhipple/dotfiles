{
  allowUnfree = true;
  allowBroken = true;
  useSandbox = true;

  hardware.pulseaudio.enable = true;

  packageOverrides = pkgs: with pkgs; {
    # Minimal set of packages to install everywhere
    minEnv = hiPrio (buildEnv {
      name = "minEnv";
      paths = [
        bashInteractive
        bc
        coreutils
        curl
        file
        gitAndTools.hub
        global
        gnused
        gnutar
        htop
        nix-repl
        nox
        par
        pass
        rlwrap
        tmux
        tree
        unzip
        wget
        zsh
      ];
    });

    # For "permanent" systems; compatible on both Mac and Linux
    bigEnv = hiPrio (buildEnv {
      name = "bigEnv";
      paths = [
        aspell
        bind
        chromium
        cmake
        emacs
        gnumake
        gnupg21
        gnutls
        graphviz
        httpie
        icu
        imagemagick
        irssi
        neovim
        nodePackages.tern  # Needed by spacemacs JS layer
        pandoc
        pdsh
        shellcheck
        source-code-pro
        stack
        upower
        vagrant
        vimPlugins.youcompleteme
        weechat
        xclip
        xsel
        zeal
        zlib
      ];
    });

    # For "permanent" systems; these packages don't seem to play well with MacOS
    bigEnvLinux = hiPrio (buildEnv {
      name = "bigEnvLinux";
      paths = [
        calibre
        haskellPackages.threadscope
        lftp
        mupdf
        vlc
      ];
    });

    pyEnv = python27.withPackages (ps: with ps; [
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
    ]);

    haskellEnv = haskellPackages.ghcWithPackages (ps: with ps; [
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
  };
}
