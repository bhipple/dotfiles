let
  rev = "1e778bc5ca91b78d33673dfaddf131a7434ab322";
  sha256 = "0svwagz0yxg2lvhx526n9hj9ggxmnkkx4y59hl55c0146sw0xxrk";
  emacs-overlay = import (builtins.fetchTarball {
    url = "https://github.com/nix-community/emacs-overlay/archive/${rev}.tar.gz";
    inherit sha256;
  });
in
  emacs-overlay
