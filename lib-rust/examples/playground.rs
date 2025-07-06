//! Your netencode playground - run with: cargo run --example playground
//! 
//! Experiment with netencode here! Try creating your own data structures.

use netencode::T;

fn main() {
    println!("🎮 Netencode Playground");
    println!("=======================");
    println!();
    
    // Try creating your own netencode structures here!
    // Some ideas:
    
    // 1. Create a user profile
    let _user = T::record(vec![
        ("username", T::text("your_name")),
        ("email", T::text("you@example.com")),
        // Add more fields...
    ]);
    
    // 2. Create a shopping cart
    let _cart = T::list(vec![
        T::record(vec![
            ("item", T::text("laptop")),
            ("quantity", T::natural(1)),
            ("price", T::natural(99999)), // in cents
        ]),
        // Add more items...
    ]);
    
    // 3. Create a tagged union for different message types
    let _message = T::tag("TextMessage", T::record(vec![
        ("content", T::text("Hello from netencode!")),
        ("timestamp", T::natural(1640995200)),
    ]));
    
    println!("Add your experiments above, then run: cargo run --example playground");
    println!("Try encoding your structures with: value.encode()");
    println!("View the encoded output with: String::from_utf8_lossy(&encoded)");
}