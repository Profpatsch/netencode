{ pkgs, lib, exact-source, ... }:

pkgs.haskellPackages.mkDerivation {
  pname = "my-prelude";
  version = "0.0.1-unreleased";

  src = exact-source ./. [
    ./my-prelude.cabal
    ./src/Aeson.hs
    ./src/Builder.hs
    ./src/Comparison.hs
    ./src/Debug.hs
    ./src/Divisive.hs
    ./src/FieldParser.hs
    ./src/Json.hs
    ./src/Json/Enc.hs
    ./src/Arg.hs
    ./src/AtLeast.hs
    ./src/MyLabel.hs
    ./src/MyPrelude.hs
    ./src/Test.hs
    ./src/Parse.hs
    ./src/Pretty.hs
    ./src/RevList.hs
    ./src/Seconds.hs
    ./src/Tool.hs
    ./src/ValidationParseT.hs
  ];

  isLibrary = true;

  libraryHaskellDepends = [
    pkgs.haskellPackages.pa-label
    pkgs.haskellPackages.pa-error-tree
    pkgs.haskellPackages.pa-pretty
    pkgs.haskellPackages.aeson-better-errors
    pkgs.haskellPackages.base64-bytestring
    pkgs.haskellPackages.contravariant
    pkgs.haskellPackages.case-insensitive
    pkgs.haskellPackages.resourcet
    pkgs.haskellPackages.foldl
    pkgs.haskellPackages.error
    pkgs.haskellPackages.hspec
    pkgs.haskellPackages.hspec-expectations-pretty-diff
    pkgs.haskellPackages.profunctors
    pkgs.haskellPackages.PyF
    pkgs.haskellPackages.semigroupoids
    pkgs.haskellPackages.these
    pkgs.haskellPackages.unliftio
    pkgs.haskellPackages.validation-selective
    pkgs.haskellPackages.vector
  ];

  license = lib.licenses.mit;

}
