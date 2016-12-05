{
  packageOverrides = pkgs_: with pkgs_; {
    all = with pkgs; hiPrio (buildEnv {
      name = "all";
      paths = [
        emacs
        clang
        neovim
        pandoc
        par
        tmux
        file
        git
      ];
    });
  };
}
