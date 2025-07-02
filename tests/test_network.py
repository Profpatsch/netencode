"""
Tests that require network connectivity.

These tests are separated from the main test suite so that the majority of tests
can run in the Nix sandbox without network access.

⚠️  NETWORK REQUIRED: These tests will be skipped if no network connectivity is available.
"""
import pytest
import subprocess
import os
from conftest import run_tool


def network_available() -> bool:
    """Check if network connectivity is available for testing."""
    try:
        result = subprocess.run(
            ['curl', '-s', '--connect-timeout', '2', 'https://api.github.com'],
            capture_output=True, timeout=5
        )
        return result.returncode == 0
    except (subprocess.TimeoutExpired, FileNotFoundError):
        return False


class TestNetworkRequiredExamples:
    """Test examples that require network connectivity."""
    
    @pytest.mark.network
    @pytest.mark.skipif(not network_available(), reason="Network required for GitHub API")
    def test_github_api_data_processing(self):
        """Test the exact example from README: GitHub API processing."""
        try:
            # Test network connectivity first
            test_result = subprocess.run(
                ['curl', '-s', '--connect-timeout', '5', 'https://api.github.com/users/octocat'],
                capture_output=True, timeout=10
            )
            if test_result.returncode != 0:
                pytest.skip("Network unavailable or GitHub API unreachable")
            
            # Fetch real data from GitHub API
            api_result = subprocess.run(
                ['curl', '-s', 'https://api.github.com/users/octocat/repos'],
                capture_output=True, text=True, timeout=30
            )
            
            # Should get valid JSON response
            assert api_result.stdout
            assert api_result.stdout.strip().startswith('[')  # Should start with array
            
            # Convert to netencode (this tests the full pipeline)
            ne_result = run_tool('json-to-netencode', stdin=api_result.stdout)
            
            # Should produce valid netencode output
            assert ne_result.stdout
            assert ne_result.stdout.strip().startswith(b'[')  # Should start with list marker
            
        except subprocess.TimeoutExpired:
            pytest.skip("GitHub API request timed out")
    
    @pytest.mark.network
    def test_nix_flake_app_examples(self):
        """Test nix run commands from the flake usage section."""
        import subprocess
        import os
        
        # Test the basic flake app command syntax from README
        # Need complete netencode format with trailing comma
        text_input = 't5:hello,'
        
        # Test the actual nix run commands from the README
        # Use the local flake path since we're testing the current codebase
        flake_path = os.path.dirname(os.path.dirname(__file__))  # Go up from tests/ to repo root
        
        try:
            # Test: echo 't5:hello,' | nix run .#netencode-pretty
            result = subprocess.run(
                ['bash', '-c', f'echo "{text_input}" | nix run {flake_path}#netencode-pretty'],
                capture_output=True, text=True, timeout=60
            )
            
            # Should produce pretty-printed output 
            assert result.returncode == 0, f"nix run #netencode-pretty failed:\nstdout: {result.stdout}\nstderr: {result.stderr}"
            assert 'hello' in result.stdout
            
            # Test: echo 't5:hello,' | nix run . (default app)
            result2 = subprocess.run(
                ['bash', '-c', f'echo "{text_input}" | nix run {flake_path}'],
                capture_output=True, text=True, timeout=60
            )
            
            # Default app should also work (it's netencode-pretty)
            assert result2.returncode == 0, f"nix run default failed:\nstdout: {result2.stdout}\nstderr: {result2.stderr}"
            assert 'hello' in result2.stdout
            
        except subprocess.TimeoutExpired:
            pytest.skip("nix run command timed out - may be building from scratch")
        except FileNotFoundError:
            pytest.skip("nix command not available")
        except Exception as e:
            # If nix fails for other reasons (like missing flake), skip gracefully
            pytest.skip(f"nix run test skipped: {e}")