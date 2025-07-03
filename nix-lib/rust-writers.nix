{ pkgs, lib, drvSeqL, ... }:

let
  # Build a rust executable, $out is the executable.
  rustSimple = args@{ name, ... }: src: pkgs.runCommandLocal "rust-simple_${name}" { } ''
    mkdir -p $out/bin
    ln -s "${rustSimpleBin args src}/bin/${name}" $out/bin/${name}
    find $out
  '';

  # Like `rustSimple`, but put the binary in `$out/bin/`.
  rustSimpleBin =
    { name
    , dependencies ? [ ]
    , doCheck ? true
    }: src:
    (if doCheck then testRustSimple else pkgs.lib.id)
      (pkgs.buildRustCrate ({
        pname = name;
        version = "1.0.0";
        crateName = name;
        crateBin = [ name ];
        dependencies = dependencies;
        src = pkgs.runCommandLocal "write-main.rs"
          {
            src = src;
            passAsFile = [ "src" ];
          } ''
          mkdir -p $out/src/bin
          cp "$srcPath" $out/src/bin/${name}.rs
          find $out
        '';
      }));

  # Build a rust library, that can be used as dependency to `rustSimple`.
  # Wrapper around `pkgs.buildRustCrate`, takes all its arguments.
  rustSimpleLib =
    { name
    , dependencies ? [ ]
    , doCheck ? true
    }: src:
    (if doCheck then testRustSimple else pkgs.lib.id)
      (pkgs.buildRustCrate ({
        pname = name;
        version = "1.0.0";
        crateName = name;
        dependencies = dependencies;
        src = pkgs.runCommandLocal "write-lib.rs"
          {
            src = src;
            passAsFile = [ "src" ];
          } ''
          mkdir -p $out/src
          cp "$srcPath" $out/src/lib.rs
          find $out
        '';
      }));

  rustFullLib = args@{ name, src, doCheck ? true, libPath ? "src/lib.rs",  dependencies ? [ ] }:
    (if doCheck then testRustSimple else pkgs.lib.id)
      (pkgs.buildRustCrate {
        pname = name;
        version = "1.0.0";
        crateName = name;
        edition = "2021";
        buildTests = true;

        inherit src;
        inherit libPath;
        inherit dependencies;
      });

  /* Takes a `buildRustCrate` derivation as an input,
    * builds it with `{ buildTests = true; }` and runs
    * all tests found in its `tests` dir. If they are
    * all successful, `$out` will point to the crate
    * built with `{ buildTests = false; }`, otherwise
    * it will fail to build.
    *
    * See also `nix.drvSeqL` which is used to implement
    * this behavior.
    */
  testRustSimple = rustDrv:
    let
      crate = buildTests: rustDrv.override { inherit buildTests; };
      tests = pkgs.runCommandLocal "${rustDrv.name}-tests-run" { }
        ''
          set -euo pipefail
          ls ${crate true}/tests
          for test in $(find "${crate true}/tests" -type f -executable); do
            echo "Running test $test"
            $test
          done
          touch "$out"
        '';
    in
    drvSeqL [ tests ] (crate false);

in
{
  inherit
    rustSimple
    rustSimpleBin
    rustSimpleLib
    rustFullLib
    ;
}
