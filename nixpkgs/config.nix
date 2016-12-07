{
  allowUnfree = true;

  packageOverrides = pkgs: with pkgs; {
    # Temporary fix for MacOS Sierra. Remove this block once the issues
    # with Jemalloc are resolved.
    neovim = pkgs.lib.overrideDerivation (pkgs.neovim.override {
      withJemalloc = false;
    }) (attrs: rec {
      name = "neovim-${version}";
      version = "0.1.7";
      src = pkgs.fetchFromGitHub {
        owner = "neovim";
        repo = "neovim";
        rev = "0542baac28681050483c685c79efcb4d3c1e32ea";
        sha256 = "0bk0raxlb1xsqyw9pmqmxvcq5szqhimidrasnvzrci84gld8cwz4";
      };
    });

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
        graphviz
        imagemagick
        irssi
        nixops
        pandoc
        shellcheck
        stack
        vagrant
        xclip
        xsel
      ];
    });

    # For "permanent" systems; compatible on both Mac and Linux
    bigEnvLinux = with pkgs; hiPrio (buildEnv {
      name = "bigEnvLinux";
      paths = [
        bigEnv
        calibre
        dropbox
        mupdf
        vlc
        lftp
        texlive-combined-full-2016
      ];
    });
  };
}
