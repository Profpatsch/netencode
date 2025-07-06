//! Basic netencode examples - run with: cargo run --example basic

use netencode::T;

fn main() {
    println!("🦀 Basic Netencode Examples");
    println!("============================");
    
    // Basic types
    let unit = T::unit();
    println!("Unit: {:?} -> {}", unit, String::from_utf8_lossy(&unit.encode()));
    
    let number = T::natural(42);
    println!("Natural: {:?} -> {}", number, String::from_utf8_lossy(&number.encode()));
    
    let integer = T::integer(-42);
    println!("Integer: {:?} -> {}", integer, String::from_utf8_lossy(&integer.encode()));
    
    let boolean = T::boolean(true);
    println!("Boolean: {:?} -> {}", boolean, String::from_utf8_lossy(&boolean.encode()));
    
    let text = T::text("Hello, World!");
    println!("Text: {:?} -> {}", text, String::from_utf8_lossy(&text.encode()));
    
    let binary = T::binary(b"binary data");
    println!("Binary: {:?} -> {}", binary, String::from_utf8_lossy(&binary.encode()));
    
    // Simple record
    let record = T::record(vec![
        ("name", T::text("Alice")),
        ("age", T::natural(30)),
    ]);
    println!("Record: {:?} -> {}", record, String::from_utf8_lossy(&record.encode()));
    
    // Simple list
    let list = T::list(vec![
        T::text("apple"),
        T::text("banana"),
        T::text("cherry"),
    ]);
    println!("List: {:?} -> {}", list, String::from_utf8_lossy(&list.encode()));
}