{
  allowUnfree = true;

  packageOverrides = pkgs: {
    # Minimal set of packages to install everywhere
    minEnv = with pkgs; hiPrio (buildEnv {
      name = "minEnv";
      paths = [
        bash
        coreutils
        curl
        emacs
        file
        git
        gnused
        gnutar
        htop
        nix-repl
        par
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
        minEnv
        cmake
        gnumake
        gnupg
        gnutls
        graphviz
        imagemagick
        irssi
        pandoc
        shellcheck
        stack
        vagrant
        xclip
        xsel
      ];
    });

    # For "permanent" systems; these packages don't seem to play well with MacOS
    bigEnvLinux = with pkgs; hiPrio (buildEnv {
      name = "bigEnvLinux";
      paths = [
        bigEnv
        calibre
        dropbox
        lftp
        mupdf
        neovim
        nixops
        texlive-combined-full-2016
        vlc
      ];
    });
  };
}
