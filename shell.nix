{ pkgs ? import <nixpkgs> {} }:
  pkgs.mkShell {
    # nativeBuildInputs is usually what you want -- tools you need to run
    nativeBuildInputs = [
      pkgs.cargo
      pkgs.rustc
      pkgs.rustfmt
      pkgs.rust-analyzer
      pkgs.libiconv
   ];

   RUST_SRC_PATH = "${pkgs.rust.packages.stable.rustPlatform.rustLibSrc}";
}