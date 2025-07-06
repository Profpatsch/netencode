# Netencode Rust Playground

Welcome to your netencode experimentation workspace! 🦀

## Getting Started

Run the examples:
```bash
cargo run --example basic      # Basic type examples
cargo run --example advanced   # Complex structures
cargo run --example playground # Your experiments
```

Run tests:
```bash
cargo test
```

## API Quick Reference

### Basic Types
- `T::unit()` - Unit value
- `T::natural(n)` - Unsigned 64-bit integer
- `T::integer(n)` - Signed 64-bit integer  
- `T::boolean(b)` - Boolean as tagged unit
- `T::text(s)` - UTF-8 string
- `T::binary(bytes)` - Raw byte data

### Composite Types
- `T::tag(name, value)` - Tagged union / sum type
- `T::record(fields)` - Key-value map (like a struct)
- `T::list(items)` - Ordered list

### Encoding
```rust
let value = T::text("hello");
let encoded = value.encode();  // Returns Vec<u8>
let as_string = String::from_utf8_lossy(&encoded);
```

## Learn More

- Check out the comprehensive test suite for more examples
- View the netencode tools: netencode-pretty, netencode-filter
- Read about the format: https://github.com/Profpatsch/netencode

Happy coding! 🚀