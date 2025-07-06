//! Advanced netencode examples - run with: cargo run --example advanced

use netencode::T;

fn main() {
    println!("🚀 Advanced Netencode Examples");
    println!("==============================");
    
    // Configuration example
    let config = T::record(vec![
        ("database", T::record(vec![
            ("host", T::text("localhost")),
            ("port", T::natural(5432)),
            ("ssl", T::boolean(true)),
        ])),
        ("cache", T::record(vec![
            ("redis_url", T::text("redis://localhost:6379")),
            ("ttl", T::natural(3600)),
        ])),
        ("features", T::list(vec![
            T::text("auth"),
            T::text("logging"),
            T::text("metrics"),
        ])),
    ]);
    
    println!("Configuration:");
    println!("{}", String::from_utf8_lossy(&config.encode()));
    println!();
    
    // API Response example
    let api_response = T::record(vec![
        ("status", T::tag("success", T::record(vec![
            ("code", T::natural(200)),
            ("message", T::text("OK")),
        ]))),
        ("data", T::list(vec![
            T::record(vec![
                ("id", T::natural(1)),
                ("name", T::text("Product A")),
                ("price", T::natural(1999)),
            ]),
            T::record(vec![
                ("id", T::natural(2)),
                ("name", T::text("Product B")),
                ("price", T::natural(2999)),
            ]),
        ])),
        ("pagination", T::record(vec![
            ("page", T::natural(1)),
            ("total", T::natural(2)),
        ])),
    ]);
    
    println!("API Response:");
    println!("{}", String::from_utf8_lossy(&api_response.encode()));
    println!();
    
    // Tagged unions (Result-like)
    let success_result = T::tag("Ok", T::text("Operation completed"));
    let error_result = T::tag("Err", T::record(vec![
        ("code", T::natural(404)),
        ("message", T::text("Not found")),
    ]));
    
    println!("Success result: {}", String::from_utf8_lossy(&success_result.encode()));
    println!("Error result: {}", String::from_utf8_lossy(&error_result.encode()));
    
    // Unicode support
    let unicode = T::record(vec![
        ("greeting", T::text("Hello 🌍")),
        ("café", T::text("coffee")),
        ("世界", T::text("world")),
    ]);
    
    println!("Unicode example: {}", String::from_utf8_lossy(&unicode.encode()));
}