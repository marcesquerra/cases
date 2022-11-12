let
  shared = import ./shared.nix;
in
  with shared;
  pkgs.mkShell {
    name = "zc-shell";
    nativeBuildInputs = [ niv rust rust-analayzer cargo-watch ];
    shellHook = ''
      export RUST_SRC_PATH="${rust-src}/lib/rustlib/src/rust/library"
    '';
  }