{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {

  buildInputs = [
    (pkgs.haskellPackages.ghcWithHoogle (h: [
      h.error
      h.pa-label
      h.pa-error-tree
      h.pa-pretty
      h.hspec
      h.hspec-expectations-pretty-diff
      h.HUnit
      h.PyF
      h.semigroupoids
      h.nonempty-containers
      h.deriving-compat
      h.text
      h.bytestring
      h.selective
      h.hedgehog
      h.attoparsec
      h.aeson-better-errors
      h.unix
      h.unliftio
      h.base64-bytestring
      h.hscolour
      h.ansi-terminal
      h.nicify-lib
      h.case-insensitive
    ]))
    pkgs.haskellPackages.cabal-install
    pkgs.haskellPackages.haskell-language-server
    pkgs.rustc
    pkgs.cargo

    # keep this line if you use bash
    pkgs.bashInteractive
  ];
}
