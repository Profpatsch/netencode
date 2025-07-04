{
  description = "Length-prefixed, type-safe data serialization format and CLI tools";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        netencode-packages = import ./default.nix { inherit pkgs; };
      in
      {
        packages = {
          default = netencode-packages.netencode;
          netencode = netencode-packages.netencode;
        };

        apps = {
          default = {
            type = "app";
            program = "${netencode-packages.netencode}/bin/netencode-pretty";
          };
          netencode-pretty = {
            type = "app";
            program = "${netencode-packages.netencode}/bin/netencode-pretty";
          };
          python = {
            type = "app";
            program = let
              pythonWithNetencode = pkgs.python3.withPackages (ps: [
                ps.ipython
              ]);
              startupScript = pkgs.writeText "netencode_startup.py" ''
                import sys
                sys.path.insert(0, '${self}/lib-python')
                import netencode as ne
                print("🐍 Python REPL with netencode pre-imported as 'ne'")
                print("Try: ne.text('hello'), ne.record([('key', ne.text('value'))])")
              '';
              pythonScript = pkgs.writeShellScript "netencode-python" ''
                export PYTHONSTARTUP="${startupScript}"
                exec ${pythonWithNetencode}/bin/ipython
              '';
            in "${pythonScript}";
          };
        };

        devShells.default = import ./shell.nix { inherit pkgs; };
      }
    );
}