#{ depot, pkgs, lib, ... }:

let
  pkgs = import <nixpkgs> { };
  lib = pkgs.lib;
  rust-writers = import ./rust-writers.nix { inherit pkgs lib drvSeqL; };
  rust-crates = import ./rust-crates.nix { inherit pkgs lib; };
  exact-source = import ./exact-source.nix { inherit pkgs lib; };
  exec-helpers = import ./exec-helpers { inherit pkgs lib rust-writers; };
  drvSeqL = import ./drvSeqL.nix { inherit pkgs lib; };
  arglib = import ./arglib/netencode.nix { inherit pkgs lib rust-writers exact-source exec-helpers gen netencode-rs; };

  netencode-rs = rust-writers.rustSimpleLib
    {
      name = "netencode";
      dependencies = [
        rust-crates.nom
        exec-helpers
      ];
    }
    (builtins.readFile ./netencode.rs);

  # netencode-hs = pkgs.haskellPackages.mkDerivation {
  #   pname = "netencode";
  #   version = "0.1.0";

  #   src = exact-source ./. [
  #     ./netencode.cabal
  #     ./Netencode.hs
  #     ./Netencode/Parse.hs
  #   ];

  #   libraryHaskellDepends = [
  #     pkgs.haskellPackages.hedgehog
  #     pkgs.haskellPackages.nonempty-containers
  #     pkgs.haskellPackages.deriving-compat
  #     pkgs.haskellPackages.data-fix
  #     pkgs.haskellPackages.bytestring
  #     pkgs.haskellPackages.attoparsec
  #     pkgs.haskellPackages.pa-label
  #     depot.users.Profpatsch.my-prelude
  #     pkgs.haskellPackages.pa-error-tree
  #   ];

  #   isLibrary = true;
  #   license = lib.licenses.mit;
  # };

  gen = import ./gen.nix { inherit lib; };

  pretty-rs = rust-writers.rustSimpleLib
    {
      name = "netencode-pretty";
      dependencies = [
        netencode-rs
      ];
    }
    (builtins.readFile ./pretty.rs);

  pretty = rust-writers.rustSimple
    {
      name = "netencode-pretty";
      dependencies = [
        netencode-rs
        pretty-rs
        exec-helpers
      ];
    } ''
    extern crate netencode;
    extern crate netencode_pretty;
    extern crate exec_helpers;

    fn main() {
      let (_, prog) = exec_helpers::args_for_exec("netencode-pretty", 0);
      let t = netencode::t_from_stdin_or_die_user_error("netencode-pretty");
      match netencode_pretty::Pretty::from_u(t.to_u()).print_multiline(&mut std::io::stdout()) {
        Ok(()) => {},
        Err(err) => exec_helpers::die_temporary("netencode-pretty", format!("could not write to stdout: {}", err))
      }
    }
  '';

  netencode-mustache = rust-writers.rustSimple
    {
      name = "netencode_mustache";
      dependencies = [
        arglib.rust
        netencode-rs
        rust-crates.mustache
      ];
    }
    (builtins.readFile ./netencode-mustache.rs);


  record-get = rust-writers.rustSimple
    {
      name = "record-get";
      dependencies = [
        netencode-rs
        exec-helpers
      ];
    } ''
    extern crate netencode;
    extern crate exec_helpers;
    use netencode::{encode, dec};
    use netencode::dec::{Decoder, DecodeError};

    fn main() {
        let args = exec_helpers::args("record-get", 1);
        let field = match std::str::from_utf8(&args[0]) {
            Ok(f) => f,
            Err(_e) => exec_helpers::die_user_error("record-get", format!("The field name needs to be valid unicode"))
        };
        let t = netencode::t_from_stdin_or_die_user_error("record-get");
        match (dec::RecordDot {field, inner: dec::AnyU }).dec(t.to_u()) {
            Ok(u) => encode(&mut std::io::stdout(), &u).expect("encoding to stdout failed"),
            Err(DecodeError(err)) => exec_helpers::die_user_error("record-get", err)
        }
    }
  '';

  record-splice-env = rust-writers.rustSimple
    {
      name = "record-splice-env";
      dependencies = [
        netencode-rs
        exec-helpers
      ];
    } ''
    extern crate netencode;
    extern crate exec_helpers;
    use netencode::dec::{Record, Try, ScalarAsBytes, Decoder, DecodeError};

    fn main() {
        let t = netencode::t_from_stdin_or_die_user_error("record-splice-env");
        let (_, prog) = exec_helpers::args_for_exec("record-splice-env", 0);
        match Record(Try(ScalarAsBytes)).dec(t.to_u()) {
            Ok(map) => {
                exec_helpers::exec_into_args(
                    "record-splice-env",
                    prog,
                    // some elements can’t be decoded as scalars, so just ignore them
                    map.into_iter().filter_map(|(k, v)| v.map(|v2| (k, v2)))
                );
            },
            Err(DecodeError(err)) => exec_helpers::die_user_error("record-splice-env", err),
        }
    }
  '';

  env-splice-record = rust-writers.rustSimple
    {
      name = "env-splice-record";
      dependencies = [
        netencode-rs
        exec-helpers
      ];
    } ''
    extern crate netencode;
    extern crate exec_helpers;
    use netencode::{T};
    use std::os::unix::ffi::OsStringExt;

    fn main() {
        exec_helpers::no_args("env-splice-record");
        let mut res = std::collections::HashMap::new();
        for (key, val) in std::env::vars_os() {
          match (String::from_utf8(key.into_vec()), String::from_utf8(val.into_vec())) {
            (Ok(k), Ok(v)) => { let _ = res.insert(k, T::Text(v)); },
            // same as in record-splice-env, we ignore non-utf8 variables
            (_, _) => {},
          }
        }
        netencode::encode(&mut std::io::stdout(), &T::Record(res).to_u()).unwrap()
    }
  '';

in
{
  inherit
    netencode-rs
    #netencode-hs
    pretty-rs
    pretty
    netencode-mustache
    record-get
    record-splice-env
    env-splice-record
    gen
    ;
}
