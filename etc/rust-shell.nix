with import <nixpkgs> {};

stdenv.mkDerivation {
  name = "rust-env";
  nativeBuildInputs = [
    rustc
    cargo
  ];
  buildInputs = [
    # Example Run-time Additional Dependencies
    rustfmt
    openssl
  ];

  RUST_BACKTRACE = 1;
}
