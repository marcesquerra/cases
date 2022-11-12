let
  shared = import ./shared.nix;
in
  with shared;
  getFromCargo {
    src = ./.;
    cargoSha256 = "sha256-/og3Mc2NPgpnhUJN5Bbhazs0azIaXTiF8A2Ti816Itc=";
  }
