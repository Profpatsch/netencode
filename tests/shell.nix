{ pkgs ? import <nixpkgs> {} }:

let
  # Import the main netencode derivation
  netencode = import ../default.nix { inherit pkgs; };
  
in pkgs.mkShell {
  buildInputs = [
    # Combined netencode package (includes all tools)
    netencode.netencode
    
    # Python with pytest for manual testing
    (pkgs.python3.withPackages (ps: with ps; [ pytest ]))
    
    # For network tests
    pkgs.curl
  ];
  
  shellHook = ''
    echo "Netencode development shell"
    echo "All tools available in PATH: json-to-netencode, netencode-filter, etc."
    echo ""
    echo "Testing:"
    echo "  pytest                    # All tests (including network)"
    echo "  pytest -m 'not network'  # Offline tests only"
    echo "  pytest test_network.py   # Network tests only"
    echo ""
    echo "For most testing, use: nix-build -A netencode-tests"
  '';
}