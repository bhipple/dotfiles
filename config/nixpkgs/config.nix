{
  # Add the Nix User Repository
  packageOverrides = pkgs: {
    nur = import (builtins.fetchTarball https://github.com/nix-community/NUR/archive/master.tar.gz) {
      inherit pkgs;
    };

    bhipple = import /home/bhipple/dotfiles/nur-packages {};
  };

  permittedInsecurePackages = [
    "python3.10-cryptography-3.4.8"
    "openssl-1.1.1u"
  ];
}
