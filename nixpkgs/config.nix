{
  allowUnfree = true;

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
        bash
        coreutils
        curl
        file
        git
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
        cmake
        gnumake
        gnupg
        gnutls
        graphviz
        haskellPackages.threadscope
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
        lftp
        mupdf
        vlc
      ];
    });
  };
}
