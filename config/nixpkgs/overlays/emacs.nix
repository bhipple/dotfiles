let
  rev = "c36610f3c986f8699a85efb9978ed602da40d4e2";
  sha256 = "1wnhy6z8zv2b00wrjrs5205ry0g8lamjb905zf14cxsgs0fz8dd6";
in
  import (builtins.fetchTarball {
    url = "https://github.com/nix-community/emacs-overlay/archive/${rev}.tar.gz";
    inherit sha256;
  })
