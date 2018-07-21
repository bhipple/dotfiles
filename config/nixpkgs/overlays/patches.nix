self: super: {

  # nix = super.nix.overrideAttrs(_: {
  #   patches = [
  #     (super.fetchpatch {
  #       # Show build output with `nix build`. https://github.com/NixOS/nix/pull/1647
  #       url = "https://github.com/NixOS/nix/commit/d02e12f366a02acf34b458eb7530baf2bdc2d251.patch";
  #       sha256 = "1vbpnnb8mmrkqpmhb92cm9v44y711cqbyfhrbfdhxf643ayz1ngl";
  #     })
  #   ];
  # });

}
