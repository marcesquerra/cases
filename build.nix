let
  shared = import ./shared.nix;
in
  with shared;
  getFromCargo {
    src = ./.;
    cargoSha256 = "sha256-TBnueYiwlS16w5cYfly7UyJhwqVTm41MAHB/si01BRU=";
  }
