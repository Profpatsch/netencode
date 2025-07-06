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
          haskell = {
            type = "app";
            program = let
              ghciWithNetencode = pkgs.haskellPackages.ghcWithHoogle (h: [
                netencode-packages.netencode-hs
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
              haskellScript = pkgs.writeShellScript "netencode-haskell" ''
                exec ${ghciWithNetencode}/bin/ghci -ghci-script ${ghciScript}
              '';
            in "${haskellScript}";
          };
          rust = {
            type = "app";
            program = let
              rustScript = pkgs.writers.writePython3 "netencode-rust" {
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
                    
                    rust_src = "${self}/lib-rust"
                    
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
            in "${rustScript}";
          };
          nix = {
            type = "app";
            program = let
              nixScript = pkgs.writeShellScript "netencode-nix" ''
                echo "❄️  Nix REPL with netencode generators in scope"
                echo "Try: text \"hello\", record [{key=\"name\"; val=text \"Alice\";}]"
                echo "Available: unit, natural, integer, boolean, text, binary, tag, record, list"
                cd ${self}
                exec ${pkgs.nix}/bin/nix repl --expr 'let ne = import ./lib-nix/gen.nix { lib = (import <nixpkgs> {}).lib; }; in ne // { pkgs = import <nixpkgs> {}; }'
              '';
            in "${nixScript}";
          };
        };

        devShells.default = import ./shell.nix { inherit pkgs; };
      }
    );
}