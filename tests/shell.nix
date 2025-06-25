{ pkgs ? import <nixpkgs> {} }:

let
  # Import the main netencode derivation
  netencode = import ../default.nix;
  
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
    netencode.record-get
    netencode.env-splice-record
    netencode.record-splice-env
    netencode.pretty
    
    # Utilities for tests
    curl  # For GitHub API tests
  ];
  
  # Set up environment variables for the tools
  shellHook = ''
    echo "Setting up netencode test environment..."
    
    # Export tool paths for both BATS and Python tests
    export JSON_TO_NETENCODE="$(which json-to-netencode)"
    export NETENCODE_FILTER="$(which netencode-filter)"
    export RECORD_GET="$(which record-get)"
    export ENV_SPLICE_RECORD="$(which env-splice-record)"
    export RECORD_SPLICE_ENV="$(which record-splice-env)"
    export NETENCODE_PRETTY="$(which netencode-pretty)"
    
    echo "Available netencode tools:"
    echo "  json-to-netencode: $JSON_TO_NETENCODE"
    echo "  netencode-filter: $NETENCODE_FILTER"
    echo "  record-get: $RECORD_GET"
    echo "  env-splice-record: $ENV_SPLICE_RECORD"
    echo "  record-splice-env: $RECORD_SPLICE_ENV"
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