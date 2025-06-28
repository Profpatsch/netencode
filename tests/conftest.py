"""
Shared pytest configuration and utilities for netencode integration tests.
"""
import os
import subprocess
import pytest
from typing import Optional, List


def get_tool_path(tool_name: str) -> str:
    """Get the full path to a netencode tool from environment variables."""
    env_var = tool_name.upper().replace('-', '_')
    path = os.environ.get(env_var)
    if path and os.path.exists(path):
        return path
    
    # Fallback: try to find in PATH
    try:
        return subprocess.check_output(['which', tool_name], text=True).strip()
    except subprocess.CalledProcessError:
        pytest.skip(f"Tool {tool_name} not found in environment or PATH")


def run_tool(tool_name: str, *args: str, stdin: Optional[str] = None, 
             expect_success: bool = True) -> subprocess.CompletedProcess:
    """
    Run a netencode tool with the given arguments and stdin.
    
    Args:
        tool_name: Name of the tool (e.g., 'record-get', 'json-to-netencode')
        *args: Command line arguments to pass to the tool
        stdin: String to pass as stdin to the tool
        expect_success: If True, raise exception on non-zero exit code
        
    Returns:
        CompletedProcess object with stdout, stderr, returncode
    """
    tool_path = get_tool_path(tool_name)
    
    result = subprocess.run(
        [tool_path] + list(args),
        input=stdin,
        text=True,
        capture_output=True
    )
    
    if expect_success and result.returncode != 0:
        pytest.fail(
            f"{tool_name} failed with exit code {result.returncode}\n"
            f"stdout: {result.stdout}\n"
            f"stderr: {result.stderr}\n"
            f"stdin: {stdin}"
        )
    
    return result


