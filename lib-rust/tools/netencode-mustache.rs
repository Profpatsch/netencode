extern crate arglib_netencode;
extern crate mustache;
extern crate netencode;

use mustache::Data;
use netencode::T;
use std::collections::HashMap;
use std::io::Read;
use std::os::unix::ffi::OsStrExt;

fn netencode_to_mustache_data_dwim(t: T) -> Data {
    match t {
        T::Unit => Data::Null,
        T::N(u) => Data::String(u.to_string()),
        T::I(i) => Data::String(i.to_string()),
        T::Text(s) => Data::String(s),
        T::Binary(_) => unimplemented!("Binary data not supported in mustache templates"),
        T::Sum(tag) => {
            // Handle booleans as special case
            match (tag.tag.as_str(), tag.val.as_ref()) {
                ("true", T::Unit) => Data::Bool(true),
                ("false", T::Unit) => Data::Bool(false),
                _ => unimplemented!("Complex tagged values not supported in mustache templates"),
            }
        },
        T::Record(xs) => Data::Map(
            xs.into_iter()
                .map(|(key, val)| (key, netencode_to_mustache_data_dwim(val)))
                .collect::<HashMap<_, _>>(),
        ),
        T::List(xs) => Data::Vec(
            xs.into_iter()
                .map(|x| netencode_to_mustache_data_dwim(x))
                .collect::<Vec<_>>(),
        ),
    }
}

pub fn from_stdin() -> () {
    let data = netencode_to_mustache_data_dwim(arglib_netencode::arglib_netencode(
        "netencode-mustache",
        Some(std::ffi::OsStr::new("TEMPLATE_DATA")),
    ));
    let mut stdin = String::new();
    std::io::stdin().read_to_string(&mut stdin).unwrap();
    mustache::compile_str(&stdin)
        .and_then(|templ| templ.render_data(&mut std::io::stdout(), &data))
        .unwrap()
}

pub fn main() {
    from_stdin()
}
