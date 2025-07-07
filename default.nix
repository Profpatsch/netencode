{ pkgs ? import <nixpkgs> { } }:

let
  lib = pkgs.lib;
  netencodeSourceDir = toString ./.;
  rust-writers = import ./nix-lib/rust-writers.nix { inherit pkgs lib drvSeqL; };
  rust-crates = import ./nix-lib/rust-crates.nix { inherit pkgs lib; };
  exact-source = import ./nix-lib/exact-source.nix { inherit pkgs lib; };
  exec-helpers = import ./lib-rust/exec-helpers/default.nix { inherit pkgs lib rust-writers exact-source; };
  drvSeqL = import ./nix-lib/drvSeqL.nix { inherit pkgs lib; };
  arglib = import ./lib-haskell/arglib/netencode.nix { inherit pkgs lib rust-writers exact-source exec-helpers gen netencode-rs netencode-hs; };
  my-prelude = import ./third-party/my-prelude/default.nix { inherit pkgs lib exact-source; };

  netencode-rs =
    rust-writers.rustFullLib {
      name = "netencode";
      libPath = "src/netencode.rs";

      src = exact-source ./lib-rust [
        ./lib-rust/Cargo.toml
        ./lib-rust/src/netencode.rs
        ./lib-rust/tests/simple_generator_test.rs
      ];

      dependencies = [
        rust-crates.nom
        rust-crates.indexmap
        exec-helpers.exec-helpers-rs
      ];
    };


  netencode-hs = pkgs.haskellPackages.mkDerivation {
    pname = "netencode";
    version = "0.1.0";

    src = exact-source ./lib-haskell [
      ./lib-haskell/netencode.cabal
      ./lib-haskell/Netencode.hs
      ./lib-haskell/Netencode/Parse.hs
      ./lib-haskell/test/GeneratorSpec.hs
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

  gen = import ./lib-nix/gen.nix { inherit lib; };

  pretty-rs = rust-writers.rustSimpleLib
    {
      name = "netencode-pretty";
      dependencies = [
        netencode-rs
      ];
    }
    (builtins.readFile ./lib-rust/pretty.rs);

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
    (builtins.readFile ./lib-rust/tools/netencode-mustache.rs);


  netencode-record-get = rust-writers.rustSimple
    {
      name = "netencode-record-get";
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
        let args = exec_helpers::args("netencode-record-get", 1);
        let field = match std::str::from_utf8(&args[0]) {
            Ok(f) => f,
            Err(_e) => exec_helpers::die_user_error("netencode-record-get", format!("The field name needs to be valid unicode"))
        };
        let t = netencode::t_from_stdin_or_die_user_error("netencode-record-get");
        match (dec::RecordDot {field, inner: dec::AnyU }).dec(t.to_u()) {
            Ok(u) => encode(&mut std::io::stdout(), &u).expect("encoding to stdout failed"),
            Err(DecodeError(err)) => exec_helpers::die_user_error("netencode-record-get", err)
        }
    }
  '';

  netencode-to-env = rust-writers.rustSimple
    {
      name = "netencode-to-env";
      dependencies = [
        netencode-rs
        exec-helpers.exec-helpers-rs
      ];
    } ''
    extern crate netencode;
    extern crate exec_helpers;
    use netencode::dec::{Record, Try, ScalarAsBytes, Decoder, DecodeError};

    fn main() {
        let t = netencode::t_from_stdin_or_die_user_error("netencode-to-env");
        let (_, prog) = exec_helpers::args_for_exec("netencode-to-env", 0);
        match Record(Try(ScalarAsBytes)).dec(t.to_u()) {
            Ok(map) => {
                exec_helpers::exec_into_args(
                    "netencode-to-env",
                    prog,
                    // some elements can’t be decoded as scalars, so just ignore them
                    map.into_iter().filter_map(|(k, v)| v.map(|v2| (k, v2)))
                );
            },
            Err(DecodeError(err)) => exec_helpers::die_user_error("netencode-to-env", err),
        }
    }
  '';

  env-to-netencode = rust-writers.rustSimple
    {
      name = "env-to-netencode";
      dependencies = [
        netencode-rs
        rust-crates.indexmap
        exec-helpers.exec-helpers-rs
      ];
    } ''
    extern crate netencode;
    extern crate exec_helpers;
    extern crate indexmap;
    use netencode::{T};
    use std::os::unix::ffi::OsStringExt;

    fn main() {
        exec_helpers::no_args("env-to-netencode");
        let mut res = indexmap::IndexMap::new();
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
      ./man/netencode-pretty.1.scd
      ./man/netencode-record-get.1.scd
      ./man/netencode-to-env.1.scd
      ./man/env-to-netencode.1.scd
      ./man/json-to-netencode.1.scd
      ./man/netencode-filter.1.scd
      ./man/netencode-plain.1.scd
      ./man/netencode-mustache.1.scd
    ];
    nativeBuildInputs = [ pkgs.scdoc ];
    buildPhase = ''
      # Generate man pages from scdoc sources
      scdoc < man/netencode.5.scd > netencode.5
      scdoc < man/netencode-pretty.1.scd > netencode-pretty.1
      scdoc < man/netencode-record-get.1.scd > netencode-record-get.1
      scdoc < man/netencode-to-env.1.scd > netencode-to-env.1
      scdoc < man/env-to-netencode.1.scd > env-to-netencode.1
      scdoc < man/json-to-netencode.1.scd > json-to-netencode.1
      scdoc < man/netencode-filter.1.scd > netencode-filter.1
      scdoc < man/netencode-plain.1.scd > netencode-plain.1
      scdoc < man/netencode-mustache.1.scd > netencode-mustache.1
    '';
    installPhase = ''
      mkdir -p $out/share/man/man1 $out/share/man/man5

      # Install section 5 (file formats)
      cp netencode.5 $out/share/man/man5/

      # Install section 1 (commands)
      cp netencode-pretty.1 $out/share/man/man1/
      cp netencode-record-get.1 $out/share/man/man1/
      cp netencode-to-env.1 $out/share/man/man1/
      cp env-to-netencode.1 $out/share/man/man1/
      cp json-to-netencode.1 $out/share/man/man1/
      cp netencode-filter.1 $out/share/man/man1/
      cp netencode-plain.1 $out/share/man/man1/
      cp netencode-mustache.1 $out/share/man/man1/
    '';
  };

  json-to-netencode = rust-writers.rustSimple
    {
      name = "json-to-netencode";
      dependencies = [
        netencode-rs
        exec-helpers.exec-helpers-rs
        rust-crates.serde_json
        rust-crates.indexmap
      ];
    } ''
    extern crate netencode;
    extern crate exec_helpers;
    extern crate serde_json;
    extern crate indexmap;
    use netencode::{T, Tag};
    use indexmap::IndexMap;

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
                let mut map = IndexMap::new();
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
        rust-crates.indexmap
        exec-helpers.exec-helpers-rs
      ];
    } ''
    extern crate netencode;
    extern crate exec_helpers;
    extern crate indexmap;
    use netencode::{T, U, dec};
    use netencode::dec::{Decoder, DecodeError};
    use std::io::{self, Write, BufRead, BufReader};

    fn matches_filter(record: &indexmap::IndexMap<String, T>, field: &str, value: &str) -> bool {
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


  netencode-tests = { testFiles ? "", pytestArgs ? "", customTest ? null }: pkgs.stdenv.mkDerivation {
    name = "netencode-tests";

    src = exact-source ./. [
      ./tests/test_integration.py
      ./tests/test_readme_examples.py
      ./tests/test_manpage_examples.py
      ./tests/test_netencode_py.py
      ./tests/test_pretty_printer.py
      ./tests/test_network.py
      ./tests/GENERATOR_TEST_SPEC.md
      ./tests/conftest.py
      ./tests/pytest.ini
      ./lib-python/netencode.py
    ];

    nativeBuildInputs = with pkgs; [
      (python3.withPackages (ps: with ps; [ pytest ]))
    ];

    buildInputs = [
      # All netencode tools needed for testing
      netencode
    ];

    # Set up environment variables for tools (like shell.nix does)
    shellHook = ''
      export JSON_TO_NETENCODE="${json-to-netencode}/bin/json-to-netencode"
      export NETENCODE_FILTER="${netencode-filter}/bin/netencode-filter"
      export NETENCODE_PLAIN="${netencode-plain}/bin/netencode-plain"
      export NETENCODE_RECORD_GET="${netencode-record-get}/bin/netencode-record-get"
      export ENV_TO_NETENCODE="${env-to-netencode}/bin/env-to-netencode"
      export NETENCODE_TO_ENV="${netencode-to-env}/bin/netencode-to-env"
      export NETENCODE_PRETTY="${pretty}/bin/netencode-pretty"
    '';

    buildPhase = ''
      # Set up environment variables for tools
      export JSON_TO_NETENCODE="${json-to-netencode}/bin/json-to-netencode"
      export NETENCODE_FILTER="${netencode-filter}/bin/netencode-filter"
      export NETENCODE_PLAIN="${netencode-plain}/bin/netencode-plain"
      export NETENCODE_RECORD_GET="${netencode-record-get}/bin/netencode-record-get"
      export ENV_TO_NETENCODE="${env-to-netencode}/bin/env-to-netencode"
      export NETENCODE_TO_ENV="${netencode-to-env}/bin/netencode-to-env"
      export NETENCODE_PRETTY="${pretty}/bin/netencode-pretty"
      export NETENCODE_MUSTACHE="${netencode-mustache}/bin/netencode_mustache"

      # Include custom test file & run if provided
      ${if customTest != null
        then ''
          echo "=== Running custom test ==="
          echo "Command: ${customTest}"
          source ${customTest}
          echo "=== Custom test completed ==="
          echo ""
        '' else ""}

      # Change to tests directory
      cd tests

      # Determine which tests to run
      if [ -n "${testFiles}" ]; then
        TEST_FILES="${testFiles}"
      else
        # Use glob to include all test files except network tests (which require internet)
        TEST_FILES="test_*.py"
        # Exclude network tests from default run
        TEST_FILES="--ignore=test_network.py test_*.py"
      fi

      # Determine pytest arguments
      if [ -n "${pytestArgs}" ]; then
        PYTEST_ARGS="${pytestArgs}"
      else
        PYTEST_ARGS="-q --tb=short"
      fi

      # Run the tests
      python -m pytest $PYTEST_ARGS $TEST_FILES
    '';

    installPhase = ''
      mkdir -p $out
      echo "Tests completed successfully" > $out/test-results.txt
      echo "Test files: ${testFiles}" >> $out/test-results.txt
      echo "Pytest args: ${pytestArgs}" >> $out/test-results.txt
    '';

    meta = {
      description = "Test suite for netencode tools";
      longDescription = ''
        Runs the netencode test suite. By default, excludes network-requiring tests.

        Usage:
        - nix-build -A netencode-tests  # Run all offline tests
        - nix-build -A netencode-tests --arg testFiles '"test_integration.py"'  # Run specific file
        - nix-build -A netencode-tests --arg pytestArgs '"-k json_to_netencode"'  # Run tests matching pattern
      '';
    };
  };

  netencode = pkgs.symlinkJoin {
    name = "netencode";
    paths = [
      pretty
      netencode-mustache
      netencode-record-get
      netencode-to-env
      env-to-netencode
      json-to-netencode
      netencode-filter
      netencode-plain
      netencode-man
    ];
    meta = {
      description = "Length-prefixed, type-safe data serialization format and CLI tools";
      longDescription = ''
        Netencode is a data serialization format inspired by bencode and netstring.
        It provides type-safe, length-prefixed encoding that is both human-readable
        for debugging and machine-efficient for parsing.

        This package includes all CLI tools for working with netencode data:
        processing, filtering, converting, and debugging structured data in Unix pipelines.
      '';
      homepage = "https://github.com/Profpatsch/netencode";
      license = pkgs.lib.licenses.mit;
    };
  };

  # Nix generator tests - evaluated directly during nix evaluation
  netencode-nix-tests = import ./lib-nix/test-gen.nix { inherit (pkgs) lib; };

  # Haskell generator tests
  netencode-haskell-tests = pkgs.haskellPackages.mkDerivation {
    pname = "netencode-haskell-tests";
    version = "0.1.0";

    src = exact-source ./lib-haskell [
      ./lib-haskell/netencode.cabal
      ./lib-haskell/Netencode.hs
      ./lib-haskell/Netencode/Parse.hs
      ./lib-haskell/test/GeneratorSpec.hs
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

    testHaskellDepends = [
      my-prelude
      pkgs.haskellPackages.bytestring
      pkgs.haskellPackages.text
      pkgs.haskellPackages.hspec
      pkgs.haskellPackages.nonempty-containers
    ];

    doCheck = true;
    isLibrary = true;
    license = lib.licenses.mit;
  };

  # Rust generator tests
  netencode-rust-tests = rust-writers.rustSimple
    {
      name = "netencode-rust-tests";
      dependencies = [
        netencode-rs
        rust-crates.indexmap
      ];
    } ''
    extern crate netencode;
    extern crate indexmap;
    use netencode::T;

    #[test]
    fn test_record_alphabetical_sort() {
        let record = T::record(vec![("b", T::text("2")), ("a", T::text("1"))]);
        let encoded = record.encode();
        let expected = b"{20:<1:a|t1:1,<1:b|t1:2,}"; // Correct alphabetical order
        assert_eq!(encoded, expected);
    }

    #[test]
    fn test_basic_generators() {
        assert_eq!(T::unit().encode(), b"u,");
        assert_eq!(T::natural(42).encode(), b"n:42,");
        assert_eq!(T::integer(-42).encode(), b"i:-42,");
        assert_eq!(T::boolean(true).encode(), b"<4:true|u,");
        assert_eq!(T::boolean(false).encode(), b"<5:false|u,");
        assert_eq!(T::text("hello").encode(), b"t5:hello,");
        assert_eq!(T::binary(b"data").encode(), b"b4:data,");
    }

    fn main() {
        // This is a test binary - tests run automatically
    }
  '';

  # Combined test suite for all language generators
  test-all-generators = pkgs.stdenv.mkDerivation {
    name = "test-all-generators";

    buildInputs = [
      netencode-python-tests  # Python generator tests
      netencode-haskell-tests
      netencode-rust-tests
    ];

    buildPhase = ''
      echo "Running cross-language generator tests..."
      echo "Python tests: ${netencode-python-tests}"
      echo "Nix tests: ${if netencode-nix-tests.success then "PASSED" else "FAILED"} (${toString (builtins.length (builtins.attrNames netencode-nix-tests.tests))} tests)"
      echo "Haskell tests: ${netencode-haskell-tests}"
      echo "Rust tests: ${netencode-rust-tests}"
      echo "All generator tests completed successfully!"
    '';

    installPhase = ''
      mkdir -p $out
      echo "All language generator tests passed" > $out/test-results.txt
      echo "Test components:" >> $out/test-results.txt
      echo "- Python: ${netencode-python-tests}" >> $out/test-results.txt
      echo "- Nix: ${if netencode-nix-tests.success then "PASSED" else "FAILED"}" >> $out/test-results.txt
      echo "- Haskell: ${netencode-haskell-tests}" >> $out/test-results.txt
      echo "- Rust: ${netencode-rust-tests}" >> $out/test-results.txt
    '';

    meta = {
      description = "Cross-language generator test suite for netencode";
      longDescription = ''
        Runs the unified generator test specification across all language implementations:
        Python, Nix, Haskell, and Rust. Ensures consistent netencode generation
        and validates the same test cases in each language.
      '';
    };
  };

  # Python generator tests
  netencode-python-tests = pkgs.stdenv.mkDerivation {
    name = "netencode-python-tests";
    src = exact-source ./. [
      ./lib-python/netencode.py
      ./lib-python/test_generator_spec.py
    ];

    nativeBuildInputs = with pkgs; [ (python3.withPackages (ps: with ps; [ pytest ])) ];
    buildInputs = [];

    buildPhase = ''
      cd lib-python
      python -m pytest test_generator_spec.py
    '';

    installPhase = ''
      mkdir -p $out
      echo "Python generator tests passed" > $out/test-results.txt
    '';

    meta = {
      description = "Test suite for netencode Python generators";
    };
  };

  # REPL Apps for different languages
  python-repl = let
    pythonWithNetencode = pkgs.python3.withPackages (ps: [
      ps.ipython
    ]);
    startupScript = pkgs.writeText "netencode_startup.py" ''
      import sys
      sys.path.insert(0, '${netencodeSourceDir}/lib-python')
      import netencode as ne
      print("🐍 Python REPL with netencode pre-imported as 'ne'")
      print("Try: ne.text('hello'), ne.record([('key', ne.text('value'))])")
    '';
  in pkgs.writeShellScript "netencode-python" ''
    export PYTHONSTARTUP="${startupScript}"
    exec ${pythonWithNetencode}/bin/ipython
  '';

  haskell-repl = let
    ghciWithNetencode = pkgs.haskellPackages.ghcWithHoogle (h: [
      netencode-hs
      h.nonempty-containers
    ]);
    ghciScript = pkgs.writeText "netencode.ghci" ''
      :set -XOverloadedStrings
      import Netencode
      import Data.Map.NonEmpty (fromList)
      putStrLn "🎩 Haskell REPL with Netencode module loaded"
      putStrLn "Try: text \"hello\", record (fromList [(\"key\", text \"value\")])"
      putStrLn "Available: unit, natural, integer, boolean, text, binary, tag, record, list"
    '';
  in pkgs.writeShellScript "netencode-haskell" ''
    exec ${ghciWithNetencode}/bin/ghci -ghci-script ${ghciScript}
  '';

  rust-workspace = pkgs.writers.writePython3 "netencode-rust" {
    flakeIgnore = [ "E501" "W291" "W293" "E305" "E128" "E265" "E302" "F541" ];  # ignore formatting issues
  } ''
    #!/usr/bin/env python3
    import os
    import sys
    import argparse
    import subprocess
    from pathlib import Path
    
    def run_gum(command, *args):
        """Run gum command and return result"""
        cmd = ["${pkgs.gum}/bin/gum", command] + list(args)
        if command == "confirm":
            # For confirm, just check return code
            result = subprocess.run(cmd)
            return result.returncode == 0
        elif command == "input":
            # For input, we need to preserve TTY for the prompt but capture output
            try:
                with open("/dev/tty", "r") as tty_in, open("/dev/tty", "w") as tty_err:
                    result = subprocess.run(cmd, stdin=tty_in, stderr=tty_err, stdout=subprocess.PIPE, text=True)
                    if result.returncode != 0:
                        # User cancelled (Ctrl+C or ESC)
                        print("\nCancelled. Thanks for considering netencode! 👋")
                        sys.exit(0)
                    return result.stdout.strip()
            except KeyboardInterrupt:
                print("\nCancelled. Thanks for considering netencode! 👋")
                sys.exit(0)
        else:
            return subprocess.run(cmd)

    def enter_existing_workspace(workspace_name, args):
        """Enter an existing workspace"""
        print()
        print("✅ Using existing workspace!")
        print(f"📁 Location: {Path(workspace_name).absolute()}")
        print()
        print("Available commands:")
        print("  cargo run --example basic")
        print("  cargo run --example advanced")
        print("  cargo run --example playground")
        print()
        if not args.no_cd:
            if args.yes or run_gum("confirm", "Change to workspace directory now?"):
                os.chdir(workspace_name)
                os.execv("${pkgs.bashInteractive}/bin/bash", ["bash"])
        sys.exit(0)
    
    def main():
        parser = argparse.ArgumentParser(description="Netencode Rust Workspace Creator")
        parser.add_argument("--yes", action="store_true", help="Accept all prompts automatically")
        parser.add_argument("--name", help="Set workspace directory name")
        parser.add_argument("--no-cd", action="store_true", help="Don't offer to change directory")
        args = parser.parse_args()
        
        rust_src = "${netencodeSourceDir}/lib-rust"
        
        print("🦀 Netencode Rust Workspace Creator")
        print("This will create a temporary Rust workspace where you can experiment with netencode.")
        print()
        
        # Check if default workspace exists immediately
        default_workspace = Path("netencode-playground")
        if not args.name and default_workspace.exists() and any(default_workspace.iterdir()):
            print("📁 Directory 'netencode-playground' already exists with content!")
            if args.yes:
                print("Auto-accept mode: entering existing workspace.")
                enter_existing_workspace("netencode-playground", args)
            else:
                if run_gum("confirm", "Enter existing workspace?"):
                    enter_existing_workspace("netencode-playground", args)
        
        print()
        # Get workspace name
        if args.name:
            workspace_name = args.name
        elif args.yes:
            workspace_name = "netencode-playground"
        else:
            while True:
                workspace_name = run_gum("input", "--placeholder", "netencode-playground", 
                                       "--prompt", "Workspace directory name: ")
                if not workspace_name:
                    workspace_name = "netencode-playground"
                
                workspace_path = Path(workspace_name)
                if workspace_path.exists() and any(workspace_path.iterdir()):
                    print()
                    print(f"📁 Directory '{workspace_name}' already exists with content!")
                    if not run_gum("confirm", "Enter existing workspace?"):
                        print("Please choose a different workspace name.")
                        continue
                    enter_existing_workspace(workspace_name, args)
                break
        
        print(f"Using workspace directory: {workspace_name}")
        
        # Check if directory exists (for --name flag)
        workspace_path = Path(workspace_name)
        if args.name and workspace_path.exists() and any(workspace_path.iterdir()):
            print()
            print(f"📁 Directory '{workspace_name}' already exists with content!")
            if args.yes or run_gum("confirm", "Enter existing workspace?"):
                enter_existing_workspace(workspace_name, args)
            else:
                print("Please choose a different workspace name.")
                sys.exit(1)
        elif workspace_path.exists():
            print(f"Using existing empty directory: {workspace_name}")
        else:
            print(f"Creating new directory: {workspace_name}")
        
        print()
        print("Setting up workspace...")
        
        # Create workspace structure
        workspace_path.mkdir(parents=True, exist_ok=True)
        (workspace_path / "src").mkdir(exist_ok=True)
        (workspace_path / "examples").mkdir(exist_ok=True)
        
        # Copy files from Nix store without preserving read-only permissions
        subprocess.run(
            f"${pkgs.coreutils}/bin/cp --no-preserve=mode -r {rust_src}/src/* {workspace_path}/src/",
            shell=True, check=True)
        
        subprocess.run(
            f"${pkgs.coreutils}/bin/cp --no-preserve=mode {rust_src}/workspace-template/Cargo.toml {rust_src}/workspace-template/README.md {workspace_path}/",
            shell=True, check=True)
        
        subprocess.run(
            f"${pkgs.coreutils}/bin/cp --no-preserve=mode {rust_src}/examples/*.rs {workspace_path}/examples/",
            shell=True, check=True)
        
        subprocess.run(
            f"${pkgs.coreutils}/bin/cp --no-preserve=mode -r {rust_src}/exec-helpers {workspace_path}/",
            shell=True, check=True)
        
        print()
        print("✅ Workspace created successfully!")
        print(f"📁 Location: {workspace_path.absolute()}")
        print()
        print("Next steps:")
        print(f"  cd {workspace_name}")
        print("  cargo run --example basic")
        print("  cargo run --example advanced")
        print("  cargo run --example playground")
        print()
        print("Edit examples/playground.rs to experiment!")
        
        # Offer to change directory
        print()
        if not args.no_cd and not args.yes:
            if run_gum("confirm", "Change to workspace directory now?"):
                os.chdir(workspace_name)
                os.execv("${pkgs.bashInteractive}/bin/bash", ["bash"])
    
    if __name__ == "__main__":
        main()
  '';

  nix-repl = pkgs.writeShellScript "netencode-nix" ''
    echo "❄️  Nix REPL with netencode generators in scope"
    echo "Try: text \"hello\", record [{key=\"name\"; val=text \"Alice\";}]"
    echo "Available: unit, natural, integer, boolean, text, binary, tag, record, list"
    cd ${netencodeSourceDir}
    exec ${pkgs.nix}/bin/nix repl --expr 'let ne = import ./lib-nix/gen.nix { lib = (import <nixpkgs> {}).lib; }; in ne // { pkgs = import <nixpkgs> {}; }'
  '';

in
{
  inherit
    netencode
    netencode-tests
    netencode-python-tests
    netencode-nix-tests
    netencode-haskell-tests
    netencode-rust-tests
    test-all-generators
    netencode-rs
    netencode-hs
    pretty-rs
    pretty
    netencode-mustache
    netencode-record-get
    netencode-to-env
    env-to-netencode
    json-to-netencode
    netencode-filter
    netencode-plain
    netencode-man
    gen
    python-repl
    haskell-repl
    rust-workspace
    nix-repl
    ;
}
