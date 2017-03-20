{
  allowUnfree = true;
  allowBroken = true;

  hardware.pulseaudio.enable = true;

  firefox = {
    enableAdobeFlash = true;
    enableGoogleTalkPlugin = true;
    ffmpegSupport = true;
  };

  packageOverrides = pkgs: {
    # Minimal set of packages to install everywhere
    minEnv = with pkgs; hiPrio (buildEnv {
      name = "minEnv";
      paths = [
        bashInteractive
        coreutils
        curl
        file
        gitAndTools.hub
        global
        gnused
        gnutar
        htop
        nix-repl
        par
        python
        tmux
        tree
        unzip
        wget
        zsh
      ];
    });

    # For "permanent" systems; compatible on both Mac and Linux
    bigEnv = with pkgs; hiPrio (buildEnv {
      name = "bigEnv";
      paths = [
        aspell
        cmake
        emacs25
        gnumake
        gnupg21
        gnutls
        graphviz
        icu
        imagemagick
        irssi
        neovim
        pandoc
        shellcheck
        stack
        vagrant
        xclip
        xsel
        zlib
      ];
    });

    # For "permanent" systems; these packages don't seem to play well with MacOS
    bigEnvLinux = with pkgs; hiPrio (buildEnv {
      name = "bigEnvLinux";
      paths = [
        calibre
        dropbox
        haskellPackages.threadscope
        lftp
        mupdf
        vlc
      ];
    });
  };
}
