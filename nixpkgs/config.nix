{
  packageOverrides = pkgs_: with pkgs_; {
    all = with pkgs; hiPrio (buildEnv {
      name = "all";
      paths = [
        clang
        cmake
        curl
        emacs
        file
        git
        gnumake
        htop
        neovim
        par
        tmux
        tree
        wget
        zsh
      ];
    });

    bigEnv = with pkgs; hiPrio (buildEnv {
      name = "bigEnv";
      paths = [
        calibre
        graphviz
        imagemagick
        irssi
        mupdf
        pandoc
        shellcheck
        vagrant
        vlc
        xclip
        xsel
#texlive-combined-full-2016
      ];
    });
  };
}
