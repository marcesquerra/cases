let
  sources = import ./nix/sources.nix;
  nixpkgs-mozilla = import sources.nixpkgs-mozilla;
  overlays = [nixpkgs-mozilla];
in
rec {
  pkgs = import sources.nixpkgs { inherit overlays ; config = { allowUnfree = true;}; };
  rustChannel = (pkgs.rustChannelOf { date = "2022-11-03"; channel = "stable"; });
  rust =
    let
      superRust = rustChannel.rust;
    in
      superRust.override{targets = [(pkgs.rust.toRustTarget pkgs.stdenv.targetPlatform) "wasm32-unknown-unknown"];}; # {targets = [pkgs.stdenv.targetPlatform];};
  rust-src = rustChannel.rust-src;
  rustPlatform = pkgs.makeRustPlatform{
      cargo = rust;
      rustc = rust;
    };
  getFromCargo = {src, cargoSha256, nativeBuildInputs ? [], cargoBuildFlags ? []} :
    let
      lib = pkgs.lib;
      asName = candidates :
        let
          ts = e: if (builtins.isAttrs e) && (builtins.hasAttr "name" e) && e.name != null then e.name else toString e;
          stringCandidates = map ts candidates;
          wholeString = lib.concatStrings stringCandidates;
        in
          builtins.hashString "sha256" wholeString;
    in
      rustPlatform.buildRustPackage rec {
        inherit src cargoSha256 nativeBuildInputs cargoBuildFlags;
        pname = "cargo-${asName [src]}";
        version = "N/A";
        doCheck = false;
      };
  rust-analayzer = getFromCargo {
    src = sources.rust-analyzer;
    cargoSha256 = "sha256-QnMa4SIcZdLn6ZLIIg8mN6RL6VATps34QXdupGk3+NE=";
  };
  cargo-watch = getFromCargo {
    src = sources.cargo-watch;
    cargoSha256 = "sha256-XXWaJv3DV2yuiWa7X7e+gINS0FBCDX5OW0xeu2XXld0=";
  };
  niv = ((import sources.niv) {}).niv;
}