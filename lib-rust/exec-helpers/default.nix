{ rust-writers, exact-source, pkgs, lib, ... }:

let
  exec-helpers-rs =
    rust-writers.rustSimpleLib
    {
      name = "exec-helpers";
    }
      (builtins.readFile ./exec_helpers.rs);

  exec-helpers-hs = pkgs.haskellPackages.mkDerivation {
    pname = "exec-helpers";
    version = "0.1.0";

    src = exact-source ./. [
      ./exec-helpers.cabal
      ./ExecHelpers.hs
    ];

    libraryHaskellDepends = [
    ];

    isLibrary = true;
    license = lib.licenses.mit;
  };

in
{
  inherit
    exec-helpers-rs
    exec-helpers-hs;
}
