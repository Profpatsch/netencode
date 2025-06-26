#{ depot, pkgs, lib, ... }:

let
  pkgs = import <nixpkgs> { };
  lib = pkgs.lib;
  rust-writers = import ./nix-lib/rust-writers.nix { inherit pkgs lib drvSeqL; };
  rust-crates = import ./nix-lib/rust-crates.nix { inherit pkgs lib; };
  exact-source = import ./nix-lib/exact-source.nix { inherit pkgs lib; };
  exec-helpers = import ./exec-helpers/default.nix { inherit pkgs lib rust-writers exact-source; };
  drvSeqL = import ./nix-lib/drvSeqL.nix { inherit pkgs lib; };
  arglib = import ./arglib/netencode.nix { inherit pkgs lib rust-writers exact-source exec-helpers gen netencode-rs netencode-hs; };
  my-prelude = import ./third-party/my-prelude/default.nix { inherit pkgs lib exact-source; };

  netencode-rs = rust-writers.rustSimpleLib
    {
      name = "netencode";
      dependencies = [
        rust-crates.nom
        exec-helpers.exec-helpers-rs
      ];
    }
    (builtins.readFile ./netencode.rs);

  netencode-hs = pkgs.haskellPackages.mkDerivation {
    pname = "netencode";
    version = "0.1.0";

    src = exact-source ./. [
      ./netencode.cabal
      ./Netencode.hs
      ./Netencode/Parse.hs
    ];

    libraryHaskellDepends = [
      my-prelude
      pkgs.haskellPackages.hedgehog
      pkgs.haskellPackages.nonempty-containers
      pkgs.haskellPackages.deriving-compat
      pkgs.haskellPackages.data-fix
      pkgs.haskellPackages.bytestring
      pkgs.haskellPackages.attoparsec
      pkgs.haskellPackages.pa-label
      pkgs.haskellPackages.pa-error-tree
    ];

    isLibrary = true;
    license = lib.licenses.mit;
  };

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
        exec-helpers.exec-helpers-rs
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
        exec-helpers.exec-helpers-rs
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
        exec-helpers.exec-helpers-rs
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
        exec-helpers.exec-helpers-rs
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

  netencode-man = pkgs.stdenv.mkDerivation {
    name = "netencode-man";
    src = exact-source ./. [
      ./man/netencode.5.scd
    ];
    nativeBuildInputs = [ pkgs.scdoc ];
    buildPhase = ''
      scdoc < man/netencode.5.scd > netencode.5
    '';
    installPhase = ''
      mkdir -p $out/share/man/man5
      cp netencode.5 $out/share/man/man5/
    '';
  };

  json-to-netencode = rust-writers.rustSimple
    {
      name = "json-to-netencode";
      dependencies = [
        netencode-rs
        exec-helpers.exec-helpers-rs
        rust-crates.serde_json
      ];
    } ''
    extern crate netencode;
    extern crate exec_helpers;
    extern crate serde_json;
    use netencode::{T, Tag};
    use std::collections::HashMap;

    fn json_to_netencode(value: serde_json::Value) -> T {
        match value {
            serde_json::Value::Null => T::Unit,
            serde_json::Value::Bool(true) => T::Sum(Tag {
                tag: "true".to_string(),
                val: Box::new(T::Unit),
            }),
            serde_json::Value::Bool(false) => T::Sum(Tag {
                tag: "false".to_string(),
                val: Box::new(T::Unit),
            }),
            serde_json::Value::Number(n) => {
                if let Some(i) = n.as_i64() {
                    T::I(i)
                } else if let Some(u) = n.as_u64() {
                    T::N(u)
                } else {
                    // Float - convert to string representation
                    T::Text(n.to_string())
                }
            },
            serde_json::Value::String(s) => T::Text(s),
            serde_json::Value::Array(arr) => {
                T::List(arr.into_iter().map(json_to_netencode).collect())
            },
            serde_json::Value::Object(obj) => {
                let mut map = HashMap::new();
                for (k, v) in obj {
                    map.insert(k, json_to_netencode(v));
                }
                T::Record(map)
            },
        }
    }

    fn main() {
        exec_helpers::no_args("json-to-netencode");
        let stdin = std::io::stdin();
        let reader = stdin.lock();
        
        match serde_json::from_reader::<_, serde_json::Value>(reader) {
            Ok(json_value) => {
                let netencode_value = json_to_netencode(json_value);
                netencode::encode(&mut std::io::stdout(), &netencode_value.to_u()).unwrap();
            },
            Err(e) => exec_helpers::die_user_error("json-to-netencode", format!("Failed to parse JSON: {}", e)),
        }
    }
  '';

  netencode-filter = rust-writers.rustSimple
    {
      name = "netencode-filter";
      dependencies = [
        netencode-rs
        exec-helpers.exec-helpers-rs
      ];
    } ''
    extern crate netencode;
    extern crate exec_helpers;
    use netencode::{T, U, dec};
    use netencode::dec::{Decoder, DecodeError};
    use std::io::{self, Write, BufRead, BufReader};

    fn matches_filter(record: &std::collections::HashMap<String, T>, field: &str, value: &str) -> bool {
        if let Some(field_value) = record.get(field) {
            match field_value {
                T::Text(s) => s == value,
                T::N(n) => value.parse::<u64>().map_or(false, |v| *n == v),
                T::I(i) => value.parse::<i64>().map_or(false, |v| *i == v),
                T::Sum(tag) => tag.tag == value,
                _ => false,
            }
        } else {
            false
        }
    }

    fn main() {
        let args = exec_helpers::args("netencode-filter", 1);
        let filter_expr = String::from_utf8(args[0].clone())
            .expect("Filter expression must be valid UTF-8");
        
        let (field, value) = if let Some(eq_pos) = filter_expr.find('=') {
            let field = &filter_expr[..eq_pos];
            let value = &filter_expr[eq_pos + 1..];
            (field, value)
        } else {
            exec_helpers::die_user_error("netencode-filter", "Filter expression must be in format 'field=value'");
        };

        let stdin = io::stdin();
        let reader = BufReader::new(stdin.lock());
        let mut stdout = io::stdout();
        
        for line in reader.lines() {
            match line {
                Ok(line_str) => {
                    if line_str.trim().is_empty() {
                        continue;
                    }
                    
                    match netencode::parse::t_t(line_str.as_bytes()) {
                        Ok((_, t)) => {
                            match &t {
                                T::Record(record) => {
                                    if matches_filter(record, field, value) {
                                        netencode::encode(&mut stdout, &t.to_u()).unwrap();
                                        stdout.flush().unwrap();
                                    }
                                },
                                _ => {
                                    // Not a record, just pass through
                                    netencode::encode(&mut stdout, &t.to_u()).unwrap();
                                    stdout.flush().unwrap();
                                }
                            }
                        },
                        Err(_) => {
                            // Invalid netencode, skip
                            continue;
                        }
                    }
                },
                Err(_) => break,
            }
        }
    }
  '';

  netencode-plain = rust-writers.rustSimple
    {
      name = "netencode-plain";
      dependencies = [
        netencode-rs
        exec-helpers.exec-helpers-rs
      ];
    } ''
    extern crate netencode;
    extern crate exec_helpers;
    use netencode::{encode};
    use std::io::{self, Write};

    fn main() {
        exec_helpers::no_args("netencode-plain");
        let t = netencode::t_from_stdin_or_die_user_error("netencode-plain");
        
        match t.to_u() {
            // Text values - output raw text content
            netencode::U::Text(s) => {
                print!("{}", s);
            },
            // Natural numbers - output as decimal
            netencode::U::N(n) => {
                print!("{}", n);
            },
            // Signed integers - output as decimal  
            netencode::U::I(i) => {
                print!("{}", i);
            },
            // Binary data - output raw bytes
            netencode::U::Binary(bytes) => {
                io::stdout().write_all(bytes).unwrap();
            },
            // Tagged values - handle booleans, pass through others
            netencode::U::Sum(tag) => {
                match (tag.tag.as_ref(), tag.val.as_ref()) {
                    ("true", netencode::U::Unit) => {
                        print!("true");
                    },
                    ("false", netencode::U::Unit) => {
                        print!("false");
                    },
                    // For other tagged values, output the original netencode
                    _ => {
                        encode(&mut std::io::stdout(), &t.to_u()).expect("encoding to stdout failed");
                    }
                }
            },
            // Records and Lists - pass through verbatim
            netencode::U::Record(_) | netencode::U::List(_) => {
                encode(&mut std::io::stdout(), &t.to_u()).expect("encoding to stdout failed");
            },
            // Unit - output nothing (empty)
            netencode::U::Unit => {
                // Output nothing for unit values
            }
        }
    }
  '';

in
{
  inherit
    netencode-rs
    netencode-hs
    pretty-rs
    pretty
    netencode-mustache
    record-get
    record-splice-env
    env-splice-record
    json-to-netencode
    netencode-filter
    netencode-plain
    netencode-man
    gen
    ;
}
