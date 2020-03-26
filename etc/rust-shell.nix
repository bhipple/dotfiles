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
    llvmPackages.clang
    llvmPackages.libclang
  ];

  LIBCLANG_PATH = "${llvmPackages.libclang}/lib";

  RUST_BACKTRACE = 1;
}
