cargoSha256 :
let
  shared = import ./shared.nix;
in
  with shared;
  getFromCargo {
    inherit cargoSha256;
    src = ./.;
  }
