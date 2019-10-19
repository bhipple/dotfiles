let
  emacs-overlay = import (builtins.fetchTarball {
    url = https://github.com/nix-community/emacs-overlay/archive/9aa3adfd62e177b74585d1f5781f1ba5972d8de6.tar.gz;
    sha256 = "12141v17qcv1kgr8akyv67crkmmz5niy93qafxc8a0bcw3xmk7dx";
  });
in
  emacs-overlay
