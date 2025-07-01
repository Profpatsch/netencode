{ pkgs ? import <nixpkgs> {} }:

let
  # Import the main netencode derivation
  netencode = import ../default.nix { inherit pkgs; };
  
  # Python environment with pytest
  pythonEnv = pkgs.python3.withPackages (ps: with ps; [
    pytest
  ]);
  
in pkgs.mkShell {
  buildInputs = with pkgs; [
    # Python testing environment
    pythonEnv
    
    # All the netencode tools
    netencode.json-to-netencode
    netencode.netencode-filter
    netencode.netencode-plain
    netencode.netencode-record-get
    netencode.env-to-netencode
    netencode.netencode-to-env
    netencode.pretty
    netencode.netencode-man
    
    # Utilities for tests
    curl  # For GitHub API tests
    man  # To read man pages
  ];
  
  # Set up environment variables for the tools
  shellHook = ''
    echo "Setting up netencode test environment..."
    
    # Export tool paths for both BATS and Python tests
    export JSON_TO_NETENCODE="$(which json-to-netencode)"
    export NETENCODE_FILTER="$(which netencode-filter)"
    export NETENCODE_PLAIN="$(which netencode-plain)"
    export NETENCODE_RECORD_GET="$(which netencode-record-get)"
    export ENV_TO_NETENCODE="$(which env-to-netencode)"
    export NETENCODE_TO_ENV="$(which netencode-to-env)"
    export NETENCODE_PRETTY="$(which netencode-pretty)"
    
    echo "Available netencode tools:"
    echo "  json-to-netencode: $JSON_TO_NETENCODE"
    echo "  netencode-filter: $NETENCODE_FILTER"
    echo "  netencode-plain: $NETENCODE_PLAIN"
    echo "  netencode-record-get: $NETENCODE_RECORD_GET"
    echo "  env-to-netencode: $ENV_TO_NETENCODE"
    echo "  netencode-to-env: $NETENCODE_TO_ENV"
    echo "  netencode-pretty: $NETENCODE_PRETTY"
    echo ""
    echo "Python testing:"
    echo "  Run all tests: pytest"
    echo "  Run with verbose output: pytest -v"
    echo "  Run specific test file: pytest test_integration.py"
    echo "  Run README examples: pytest test_readme_examples.py"
    echo "  Run specific test: pytest test_readme_examples.py::TestReadmeExamples::test_basic_record_field_extraction"
    echo "  Run tests matching pattern: pytest -k 'json_to_netencode'"
  '';
}