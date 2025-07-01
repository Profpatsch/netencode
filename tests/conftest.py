"""
Shared pytest configuration and utilities for netencode integration tests.
"""
import os
import subprocess
import pytest
from typing import Optional, List, Union


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


def run_tool(tool_name: str, *args: str, stdin: Optional[Union[str, bytes]] = None, 
             expect_success: bool = True) -> subprocess.CompletedProcess:
    """
    Run a netencode tool with the given arguments and stdin.
    
    Args:
        tool_name: Name of the tool (e.g., 'netencode-record-get', 'json-to-netencode')
        *args: Command line arguments to pass to the tool
        stdin: String or bytes to pass as stdin to the tool
        expect_success: If True, raise exception on non-zero exit code
        
    Returns:
        CompletedProcess object with stdout, stderr, returncode
    """
    tool_path = get_tool_path(tool_name)
    
    # Convert stdin to bytes if needed
    if isinstance(stdin, str):
        stdin_bytes = stdin.encode('utf-8')
    elif isinstance(stdin, bytes):
        stdin_bytes = stdin
    else:
        stdin_bytes = None
    
    result = subprocess.run(
        [tool_path] + list(args),
        input=stdin_bytes,
        capture_output=True
    )
    
    if expect_success and result.returncode != 0:
        pytest.fail(
            f"{tool_name} failed with exit code {result.returncode}\n"
            f"stdout: {result.stdout.decode('utf-8', errors='replace')}\n"
            f"stderr: {result.stderr.decode('utf-8', errors='replace')}\n"
            f"stdin: {stdin}"
        )
    
    return result


