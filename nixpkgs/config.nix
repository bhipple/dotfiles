{
  packageOverrides = pkgs_: with pkgs_; {
    all = with pkgs; lib.hiPrio (buildEnv {
      name = "all";
      ignoreCollisions = true;
      paths = [
        emacs
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
