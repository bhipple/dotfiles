{
  allowUnfree = false;
  allowUnfreePredicate = pkg: builtins.elem pkg.pname [
    "discord"
    "steam"
  ];

  # Add the Nix User Repository
  packageOverrides = pkgs: {
    nur = import (builtins.fetchTarball https://github.com/nix-community/NUR/archive/master.tar.gz) {
      inherit pkgs;
    };
  };
}
