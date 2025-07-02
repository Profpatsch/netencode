"""
Python module for constructing netencode values directly.

This module provides functions to construct netencode format values without
going through JSON conversion, allowing precise control over field ordering
and avoiding JSON parser limitations.

All functions return bytes objects since netencode is a binary format.
"""

from typing import Union, Dict, List, Any
from collections import OrderedDict


def unit() -> bytes:
    """Create a netencode unit value."""
    return b"u,"


def natural(value: int) -> bytes:
    """Create a netencode natural number (unsigned 64-bit)."""
    if value < 0:
        raise ValueError(f"Natural numbers must be non-negative, got {value}")
    if value > (2**64 - 1):
        raise ValueError(f"Natural number too large for 64-bit: {value}")
    return f"n:{value},".encode('utf-8')


def integer(value: int) -> bytes:
    """Create a netencode signed integer (64-bit)."""
    if value < -(2**63) or value > (2**63 - 1):
        raise ValueError(f"Integer out of 64-bit signed range: {value}")
    return f"i:{value},".encode('utf-8')


def boolean(value: bool) -> bytes:
    """Create a netencode boolean value."""
    if value:
        return b"<4:true|u,"
    else:
        return b"<5:false|u,"


def text(value: str) -> bytes:
    """Create a netencode text string (UTF-8)."""
    encoded = value.encode('utf-8')
    return f"t{len(encoded)}:".encode('utf-8') + encoded + b","


def binary(value: bytes) -> bytes:
    """Create a netencode binary value."""
    return f"b{len(value)}:".encode('utf-8') + value + b","


def tag(tag_name: str, value: bytes) -> bytes:
    """Create a netencode tagged value."""
    tag_encoded = tag_name.encode('utf-8')
    return f"<{len(tag_encoded)}:".encode('utf-8') + tag_encoded + b"|" + value


def record(fields: Union[Dict[str, bytes], OrderedDict, List[tuple]]) -> bytes:
    """
    Create a netencode record from fields.
    
    Args:
        fields: Can be:
            - Dict[str, bytes]: Field names to netencode values
            - OrderedDict: Preserves field order
            - List[tuple]: List of (key, value) pairs for explicit ordering
    """
    if isinstance(fields, dict) and not isinstance(fields, OrderedDict):
        # Convert regular dict to list of tuples sorted by key for consistency
        field_items = sorted(fields.items())
    elif isinstance(fields, OrderedDict):
        field_items = __builtins__['list'](fields.items())
    elif isinstance(fields, __builtins__['list']):
        field_items = fields
    else:
        raise TypeError("fields must be dict, OrderedDict, or list of tuples")
    
    # Build the content by tagging each field
    content = b""
    for key, value in field_items:
        content += tag(key, value)
    
    # Calculate total byte length of content and wrap in record syntax
    content_bytes = len(content)
    return f"{{{content_bytes}:".encode('utf-8') + content + b"}"


def list(values: List[bytes]) -> bytes:
    """Create a netencode list from values."""
    content = b"".join(values)
    content_bytes = len(content)
    return f"[{content_bytes}:".encode('utf-8') + content + b"]"



# Convenience functions for common patterns
def record_ordered(*field_pairs) -> bytes:
    """
    Create a record with explicit field ordering.
    
    Args:
        *field_pairs: Pairs of (key, value) arguments
        
    Example:
        record_ordered("zebra", text("first"), "alpha", text("second"))
    """
    if len(field_pairs) % 2 != 0:
        raise ValueError("field_pairs must be even number of arguments (key, value pairs)")
    
    fields = []
    for i in range(0, len(field_pairs), 2):
        key = field_pairs[i]
        value = field_pairs[i + 1]
        fields.append((key, value))
    
    return record(fields)


def simple_record(**kwargs) -> bytes:
    """
    Create a simple record using keyword arguments.
    Field order will be alphabetical (sorted by key).
    
    Example:
        simple_record(name=text("Alice"), age=natural(30))
    """
    return record(kwargs)