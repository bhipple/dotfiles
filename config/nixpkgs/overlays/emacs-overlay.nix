let
  rev = "dfa1b5bb41b110cf87d3f64ca0d972cd68ff1345";
  sha256 = "0fnxm1jz38wb0x8xhkyf40qnkinkml7mcrcajfha5aa6xh2xffx6";
  emacs-overlay = import (builtins.fetchTarball {
    url = "https://github.com/nix-community/emacs-overlay/archive/${rev}.tar.gz";
    inherit sha256;
  });
in
  emacs-overlay
