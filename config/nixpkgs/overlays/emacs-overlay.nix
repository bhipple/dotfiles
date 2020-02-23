let
  rev = "c7e05b69d6d86c58ed9da02163e9b5a4b253b23e";
  sha256 = "1w0hxq1xwjhhpzx2vn2c7ax58m42qnycq3c0mqi3946v8frpyvk6";
  emacs-overlay = import (builtins.fetchTarball {
    url = "https://github.com/nix-community/emacs-overlay/archive/${rev}.tar.gz";
    inherit sha256;
  });
in
  emacs-overlay
