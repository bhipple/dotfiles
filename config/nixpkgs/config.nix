{
  # Add the Nix User Repository
  packageOverrides = pkgs: {
    nur = import (builtins.fetchTarball https://github.com/nix-community/NUR/archive/master.tar.gz) {
      inherit pkgs;
    };

    bhipple = import /home/bhipple/dotfiles/nur-packages {};
  };

  permittedInsecurePackages = [
    "openssl-1.1.1w"
  ];
}
