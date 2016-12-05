{
  allowUnfree = true;

  packageOverrides = pkgs_: with pkgs_; {
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
        gnutar
        htop
        lftp
        neovim
        nix-repl
        par
        tmux
        tree
        unzip
        wget
        zsh
      ];
    });

    # For "permanent" systems
    bigEnv = with pkgs; hiPrio (buildEnv {
      name = "bigEnv";
      paths = [
        minEnv
        calibre
        cmake
        dropbox
        gnumake
        graphviz
        imagemagick
        irssi
        mupdf
        nixops
        pandoc
        shellcheck
        stack
        vagrant
        vlc
        xclip
        xsel
#texlive-combined-full-2016
      ];
    });
  };
}
