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
        };

        devShells.default = import ./shell.nix { inherit pkgs; };
      }
    );
}