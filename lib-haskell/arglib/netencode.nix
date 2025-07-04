{ pkgs, lib, rust-writers, exact-source, gen, exec-helpers, netencode-rs, netencode-hs, ... }:

let

  # Add the given nix arguments to the program as ARGLIB_NETENCODE envvar
  #
  # Calls `netencode.gen.dwim` on the provided nix args value.
  with-args = name: args: prog: pkgs.writeShellScriptBin "${name}-with-args" ''
    export ARGLIB_NETENCODE="${gen.dwim args}"
    exec ${prog} "$@"
  '';

  rust = rust-writers.rustSimpleLib
    {
      name = "arglib-netencode";
      dependencies = [
        exec-helpers.exec-helpers-rs
        netencode-rs
      ];
    } ''
    extern crate netencode;
    extern crate exec_helpers;

    use netencode::{T};
    use std::os::unix::ffi::OsStrExt;

    pub fn arglib_netencode(prog_name: &str, env: Option<&std::ffi::OsStr>) -> T {
        let env = match env {
            None => std::ffi::OsStr::from_bytes("ARGLIB_NETENCODE".as_bytes()),
            Some(a) => a
        };
        let t = match std::env::var_os(env) {
            None => exec_helpers::die_user_error(prog_name, format!("could not read args, envvar {} not set", env.to_string_lossy())),
            // TODO: good error handling for the different parser errors
            Some(soup) => match netencode::parse::t_t(soup.as_bytes()) {
                Ok((remainder, t)) => match remainder.is_empty() {
                    true => t,
                    false => exec_helpers::die_environment_problem(prog_name, format!("arglib: there was some unparsed bytes remaining: {:?}", remainder))
                },
                Err(err) => exec_helpers::die_environment_problem(prog_name, format!("arglib parsing error: {:?}", err))
            }
        };
        std::env::remove_var(env);
        t
    }
  '';

  haskell = pkgs.haskellPackages.mkDerivation {
    pname = "arglib-netencode";
    version = "0.1.0";

    src = exact-source ./. [
      ./arglib-netencode.cabal
      ./ArglibNetencode.hs
    ];

    libraryHaskellDepends = [
      pkgs.haskellPackages.pa-label
      pkgs.haskellPackages.pa-error-tree
      netencode-hs
      exec-helpers.exec-helpers-hs
    ];

    isLibrary = true;
    license = lib.licenses.mit;


  };


in
{
  inherit
    with-args
    rust
    haskell
    ;
}
