with import <nixpkgs> {};

stdenv.mkDerivation {
  name = "rust-env";
  nativeBuildInputs = [
    rustc
    cargo
  ];
  buildInputs = [
    openssl
    pkg-config
    rustfmt
  ];

  RUST_BACKTRACE = 1;
}
