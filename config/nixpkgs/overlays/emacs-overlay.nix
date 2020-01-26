let
  rev = "9d8afeb44885fe2959dbf8bed0ff9ef4570f21a8";
  sha256 = "001aw25pzn8n5qdhq21nkyzjj9fp7ykaqsl41wpsd65sh7w8a1yr";
  emacs-overlay = import (builtins.fetchTarball {
    url = "https://github.com/nix-community/emacs-overlay/archive/${rev}.tar.gz";
    inherit sha256;
  });
in
  emacs-overlay
