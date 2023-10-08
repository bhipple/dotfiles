{
  # Add the Nix User Repository
  packageOverrides = pkgs: {
    nur = import (builtins.fetchTarball https://github.com/nix-community/NUR/archive/master.tar.gz) {
      inherit pkgs;
    };

    bhipple = import /home/bhipple/dotfiles/nur-packages {};
  };

  permittedInsecurePackages = [
    "openssl-1.1.1v"
    "openssl-1.1.1w"
    "python3.10-cryptography-3.4.8"
    "python3.10-cryptography-40.0.1"
    "python3.10-requests-2.28.2"
  ];
}
