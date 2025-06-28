"""
Python module for constructing netencode values directly.

This module provides functions to construct netencode format values without
going through JSON conversion, allowing precise control over field ordering
and avoiding JSON parser limitations.
"""

from typing import Union, Dict, List, Any, OrderedDict
from collections import OrderedDict


def unit() -> str:
    """Create a netencode unit value."""
    return "u,"


def natural(value: int) -> str:
    """Create a netencode natural number (unsigned 64-bit)."""
    if value < 0:
        raise ValueError(f"Natural numbers must be non-negative, got {value}")
    if value > (2**64 - 1):
        raise ValueError(f"Natural number too large for 64-bit: {value}")
    return f"n:{value},"


def integer(value: int) -> str:
    """Create a netencode signed integer (64-bit)."""
    if value < -(2**63) or value > (2**63 - 1):
        raise ValueError(f"Integer out of 64-bit signed range: {value}")
    return f"i:{value},"


def boolean(value: bool) -> str:
    """Create a netencode boolean value."""
    if value:
        return "<4:true|u,"
    else:
        return "<5:false|u,"


def text(value: str) -> str:
    """Create a netencode text string (UTF-8)."""
    encoded = value.encode('utf-8')
    return f"t{len(encoded)}:{value},"


def binary(value: bytes) -> str:
    """Create a netencode binary value."""
    return f"b{len(value)}:{value.decode('latin1')},"


def tag(tag_name: str, value: str) -> str:
    """Create a netencode tagged value."""
    tag_encoded = tag_name.encode('utf-8')
    return f"<{len(tag_encoded)}:{tag_name}|{value}"


def record(fields: Union[Dict[str, str], OrderedDict, List[tuple]]) -> str:
    """
    Create a netencode record from fields.
    
    Args:
        fields: Can be:
            - Dict[str, str]: Field names to netencode values
            - OrderedDict: Preserves field order
            - List[tuple]: List of (key, value) pairs for explicit ordering
    """
    if isinstance(fields, dict) and not isinstance(fields, OrderedDict):
        # Convert regular dict to list of tuples sorted by key for consistency
        field_items = sorted(fields.items())
    elif isinstance(fields, OrderedDict):
        field_items = list(fields.items())
    elif isinstance(fields, list):
        field_items = fields
    else:
        raise TypeError("fields must be dict, OrderedDict, or list of tuples")
    
    # Build the content by tagging each field
    content = ""
    for key, value in field_items:
        content += tag(key, value)
    
    # Calculate total byte length of content and wrap in record syntax
    content_bytes = len(content.encode('utf-8'))
    return f"{{{content_bytes}:{content}}}"


def list_values(values: List[str]) -> str:
    """Create a netencode list from values."""
    content = "".join(values)
    content_bytes = len(content.encode('utf-8'))
    return f"[{content_bytes}:{content}]"


# Convenience functions for common patterns
def record_ordered(*field_pairs) -> str:
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


def simple_record(**kwargs) -> str:
    """
    Create a simple record using keyword arguments.
    Field order will be alphabetical (sorted by key).
    
    Example:
        simple_record(name=text("Alice"), age=natural(30))
    """
    return record(kwargs)