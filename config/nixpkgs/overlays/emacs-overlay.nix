let
  rev = "10ea3aaa7e03d5dd8c4d50b7d0ba6a566ac441d1";
  sha256 = "0vrnpj4yw6zdcdkwigdmnxlgp3qfdbzl1w4rf9ly428b2i3qvlw5";
  emacs-overlay = import (builtins.fetchTarball {
    url = "https://github.com/nix-community/emacs-overlay/archive/${rev}.tar.gz";
    inherit sha256;
  });
in
  emacs-overlay
