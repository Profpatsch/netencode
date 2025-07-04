extern crate exec_helpers;
extern crate nom;
extern crate indexmap;

use indexmap::IndexMap;
use std::fmt::{Debug, Display};
use std::io::{Read, Write};


#[derive(Debug, PartialEq, Eq, Clone)]
pub enum T {
    // Unit
    Unit,
    // Naturals (64-bit)
    N(u64),
    // Integers (64-bit)
    I(i64),
    // Text
    // TODO: make into &str
    Text(String),
    // TODO: rename to Bytes
    Binary(Vec<u8>),
    // Tags
    // TODO: make into &str
    // TODO: rename to Tag
    Sum(Tag<String, T>),
    // TODO: make into &str
    Record(IndexMap<String, T>),
    List(Vec<T>),
}

impl T {
    /// Create a unit value
    pub fn unit() -> T {
        T::Unit
    }

    /// Create a natural number (unsigned 64-bit)
    pub fn natural(n: u64) -> T {
        T::N(n)
    }

    /// Create a signed integer (64-bit)
    pub fn integer(n: i64) -> T {
        T::I(n)
    }

    /// Create a boolean value as a tagged unit
    pub fn boolean(b: bool) -> T {
        if b {
            T::tag("true", T::unit())
        } else {
            T::tag("false", T::unit())
        }
    }

    /// Create a text string (UTF-8)
    pub fn text<S: Into<String>>(s: S) -> T {
        T::Text(s.into())
    }

    /// Create binary data
    pub fn binary<B: Into<Vec<u8>>>(data: B) -> T {
        T::Binary(data.into())
    }

    /// Create a tagged value
    pub fn tag<S: Into<String>>(name: S, value: T) -> T {
        T::Sum(Tag {
            tag: name.into(),
            val: Box::new(value),
        })
    }

    /// Create a record from key-value pairs
    pub fn record<K, I>(fields: I) -> T
    where
        K: Into<String>,
        I: IntoIterator<Item = (K, T)>,
    {
        let mut map = fields.into_iter()
            .map(|(k, v)| (k.into(), v))
            .collect::<IndexMap<String, T>>();
        // Sort keys alphabetically for consistent output
        map.sort_by(|k1, _, k2, _| k1.cmp(k2));
        T::Record(map)
    }

    /// Create a list from values
    pub fn list<I: IntoIterator<Item = T>>(items: I) -> T {
        T::List(items.into_iter().collect())
    }

    pub fn to_u<'a>(&'a self) -> U<'a> {
        match self {
            T::Unit => U::Unit,
            T::N(u) => U::N(*u),
            T::I(i) => U::I(*i),
            T::Text(t) => U::Text(t.as_str()),
            T::Binary(v) => U::Binary(v),
            T::Sum(Tag { tag, val }) => U::Sum(Tag {
                tag: tag.as_str(),
                val: Box::new(val.to_u()),
            }),
            T::Record(map) => U::Record(map.iter().map(|(k, v)| (k.as_str(), v.to_u())).collect()),
            T::List(l) => U::List(l.iter().map(|v| v.to_u()).collect::<Vec<U<'a>>>()),
        }
    }

    pub fn encode<'a>(&'a self) -> Vec<u8> {
        match self {
            // TODO: don’t go via U, inefficient
            o => o.to_u().encode(),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum U<'a> {
    Unit,
    // Naturals (64-bit)
    N(u64),
    // Integers (64-bit)
    I(i64),
    // Text
    Text(&'a str),
    Binary(&'a [u8]),
    // TODO: the U-recursion we do here means we can’t be breadth-lazy anymore
    // like we originally planned; maybe we want to go `U<'a>` → `&'a [u8]` again?
    // Tags
    // TODO: rename to Tag
    Sum(Tag<&'a str, U<'a>>),
    Record(IndexMap<&'a str, U<'a>>),
    List(Vec<U<'a>>),
}

impl<'a> U<'a> {
    pub fn encode(&self) -> Vec<u8> {
        let mut c = std::io::Cursor::new(vec![]);
        encode(&mut c, self);
        c.into_inner()
    }

    pub fn to_t(&self) -> T {
        match self {
            U::Unit => T::Unit,
            U::N(u) => T::N(*u),
            U::I(i) => T::I(*i),
            U::Text(t) => T::Text((*t).to_owned()),
            U::Binary(v) => T::Binary((*v).to_owned()),
            U::Sum(Tag { tag, val }) => T::Sum(Tag {
                tag: (*tag).to_owned(),
                val: Box::new(val.to_t()),
            }),
            U::Record(map) => T::Record(
                map.iter()
                    .map(|(k, v)| ((*k).to_owned(), v.to_t()))
                    .collect::<IndexMap<String, T>>(),
            ),
            U::List(l) => T::List(l.iter().map(|v| v.to_t()).collect::<Vec<T>>()),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Tag<S, A> {
    // TODO: make into &str
    pub tag: S,
    pub val: Box<A>,
}

impl<S, A> Tag<S, A> {
    fn map<F, B>(self, f: F) -> Tag<S, B>
    where
        F: Fn(A) -> B,
    {
        Tag {
            tag: self.tag,
            val: Box::new(f(*self.val)),
        }
    }
}

fn encode_tag<W: Write>(w: &mut W, tag: &str, val: &U) -> std::io::Result<()> {
    write!(w, "<{}:{}|", tag.len(), tag)?;
    encode(w, val)?;
    Ok(())
}

pub fn encode<W: Write>(w: &mut W, u: &U) -> std::io::Result<()> {
    match u {
        U::Unit => write!(w, "u,"),
        U::N(n) => write!(w, "n:{},", n),
        U::I(i) => write!(w, "i:{},", i),
        U::Text(s) => {
            write!(w, "t{}:", s.len());
            w.write_all(s.as_bytes());
            write!(w, ",")
        }
        U::Binary(s) => {
            write!(w, "b{}:", s.len());
            w.write_all(&s);
            write!(w, ",")
        }
        U::Sum(Tag { tag, val }) => encode_tag(w, tag, val),
        U::Record(m) => {
            let mut c = std::io::Cursor::new(vec![]);
            for (k, v) in m {
                encode_tag(&mut c, k, v)?;
            }
            write!(w, "{{{}:", c.get_ref().len())?;
            w.write_all(c.get_ref())?;
            write!(w, "}}")
        }
        U::List(l) => {
            let mut c = std::io::Cursor::new(vec![]);
            for u in l {
                encode(&mut c, u)?;
            }
            write!(w, "[{}:", c.get_ref().len())?;
            w.write_all(c.get_ref())?;
            write!(w, "]")
        }
    }
}

pub fn text(s: String) -> T {
    T::Text(s)
}

pub fn t_from_stdin_or_die_user_error<'a>(prog_name: &'_ str) -> T {
    match t_from_stdin_or_die_user_error_with_rest(prog_name, &vec![]) {
        None => exec_helpers::die_user_error(prog_name, "stdin was empty"),
        Some((rest, t)) => {
            // Allow trailing newlines but reject other content
            let trimmed_rest: Vec<u8> = rest.iter()
                .cloned()
                .filter(|&b| b != b'\n')
                .collect();

            if trimmed_rest.is_empty() {
                t
            } else {
                exec_helpers::die_user_error(
                    prog_name,
                    format!(
                        "stdin contained some soup after netencode value: {:?}",
                        String::from_utf8_lossy(&rest)
                    ),
                )
            }
        }
    }
}


/// Read a netencode value from stdin incrementally, return bytes that could not be read.
/// Nothing if there was nothing to read from stdin & no initial_bytes were provided.
/// These can be passed back as `initial_bytes` if more values should be read.
pub fn t_from_stdin_or_die_user_error_with_rest<'a>(
    prog_name: &'_ str,
    initial_bytes: &[u8],
) -> Option<(Vec<u8>, T)> {
    let mut chonker = Chunkyboi::new(std::io::stdin().lock(), 4096);
    // The vec to pass to the parser on each step
    let mut parser_vec: Vec<u8> = initial_bytes.to_vec();
    // whether stdin was already empty
    let mut was_empty: bool = false;
    loop {
        match chonker.next() {
            None => {
                if parser_vec.is_empty() {
                    return None;
                } else {
                    was_empty = true
                }
            }
            Some(Err(err)) => exec_helpers::die_temporary(
                prog_name,
                &format!("could not read from stdin: {:?}", err),
            ),
            Some(Ok(mut new_bytes)) => parser_vec.append(&mut new_bytes),
        }

        match parse::t_t(&parser_vec) {
            Ok((rest, t)) => return Some((rest.to_owned(), t)),
            Err(nom::Err::Incomplete(Needed)) => {
                if was_empty {
                    exec_helpers::die_user_error(
                        prog_name,
                        &format!(
                            "unable to parse netencode from stdin, input incomplete: {}",
                            String::from_utf8_lossy(&parser_vec)
                        ),
                    );
                }
                // read more from stdin and try parsing again
                continue;
            }
            Err(err) => {
                let error_msg = match &err {
                    nom::Err::Error((input, kind)) | nom::Err::Failure((input, kind)) => {
                        format!("Parse error at: '{}' (kind: {:?})", String::from_utf8_lossy(input), kind)
                    }
                    nom::Err::Incomplete(_) => {
                        format!("Incomplete input: {:?}", err)
                    }
                };
                exec_helpers::die_user_error(
                    prog_name,
                    &format!("unable to parse netencode from stdin: {}", error_msg),
                );
            },
        }
    }
}

// iter helper
// TODO: put into its own module
struct Chunkyboi<T> {
    inner: T,
    buf: Vec<u8>,
}

impl<R: Read> Chunkyboi<R> {
    fn new(inner: R, chunksize: usize) -> Self {
        let buf = vec![0; chunksize];
        Chunkyboi { inner, buf }
    }
}

impl<R: Read> Iterator for Chunkyboi<R> {
    type Item = std::io::Result<Vec<u8>>;

    fn next(&mut self) -> Option<std::io::Result<Vec<u8>>> {
        match self.inner.read(&mut self.buf) {
            Ok(0) => None,
            Ok(read) => {
                // clone a new buffer so we can reuse the internal one
                Some(Ok(self.buf[..read].to_owned()))
            }
            Err(err) => Some(Err(err)),
        }
    }
}

pub mod parse {
    use super::{Tag, T, U};
    use indexmap::IndexMap;

        use std::ops::Neg;
    use std::str::FromStr;

    use nom::branch::alt;
    use nom::bytes::streaming::{tag, take};
    use nom::character::streaming::{char, digit1};
    use nom::combinator::{flat_map, map, map_parser, map_res, opt};
    use nom::error::{context, ErrorKind, ParseError};
    use nom::sequence::tuple;
    use nom::IResult;

    fn unit_t(s: &[u8]) -> IResult<&[u8], ()> {
        let (s, _) = context("unit", tag("u,"))(s)?;
        Ok((s, ()))
    }

    fn usize_t(s: &[u8]) -> IResult<&[u8], usize> {
        context(
            "usize",
            map_res(map_res(digit1, |n| std::str::from_utf8(n)), |s| {
                s.parse::<usize>()
            }),
        )(s)
    }

    fn sized(begin: char, end: char) -> impl Fn(&[u8]) -> IResult<&[u8], &[u8]> {
        move |s: &[u8]| {
            // This is the point where we check the descriminator;
            // if the beginning char does not match, we can immediately return.
            let (s, _) = char(begin)(s)?;
            let (s, (len, _)) = tuple((usize_t, char(':')))(s)?;
            let (s, (res, _)) = tuple((take(len), char(end)))(s)?;
            Ok((s, res))
        }
    }

    fn uint_t<'a, I: FromStr + 'a>(t: &'static str) -> impl Fn(&'a [u8]) -> IResult<&'a [u8], I> {
        move |s: &'a [u8]| {
            let (s, (_, _, int, _)) = tuple((
                tag(t.as_bytes()),
                char(':'),
                map_res(map_res(digit1, |n: &[u8]| std::str::from_utf8(n)), |s| {
                    s.parse::<I>()
                }),
                char(','),
            ))(s)?;
            Ok((s, int))
        }
    }


    fn bool_t<'a>() -> impl Fn(&'a [u8]) -> IResult<&'a [u8], bool> {
        context(
            "bool",
            alt((
                map(tag("<5:false|u,"), |_| false),
                map(tag("<4:true|u,"), |_| true)
            )),
        )
    }

    fn int_t<'a, I: FromStr + Neg<Output = I>>(
        t: &'static str,
    ) -> impl Fn(&'a [u8]) -> IResult<&[u8], I> {
        context(t, move |s: &'a [u8]| {
            let (s, (_, _, neg, int, _)) = tuple((
                tag(t.as_bytes()),
                char(':'),
                opt(char('-')),
                map_res(map_res(digit1, |n: &[u8]| std::str::from_utf8(n)), |s| {
                    s.parse::<I>()
                }),
                char(','),
            ))(s)?;
            let res = match neg {
                Some(_) => -int,
                None => int,
            };
            Ok((s, res))
        })
    }

    fn tag_t(s: &[u8]) -> IResult<&[u8], Tag<String, T>> {
        // recurses into the main parser
        map(tag_g(t_t), |Tag { tag, val }| Tag {
            tag: tag.to_string(),
            val,
        })(s)
    }

    fn tag_g<'a, P, O>(inner: P) -> impl Fn(&'a [u8]) -> IResult<&'a [u8], Tag<&'a str, O>>
    where
        P: Fn(&'a [u8]) -> IResult<&'a [u8], O>,
    {
        move |s: &[u8]| {
            let (s, tag) = sized('<', '|')(s)?;
            let (s, val) = inner(s)?;
            Ok((
                s,
                Tag {
                    tag: std::str::from_utf8(tag)
                        .map_err(|_| nom::Err::Failure((s, ErrorKind::Char)))?,
                    val: Box::new(val),
                },
            ))
        }
    }

    /// parse text scalar (`t5:hello,`)
    fn text(s: &[u8]) -> IResult<&[u8], T> {
        let (s, res) = text_g(s)?;
        Ok((s, T::Text(res.to_string())))
    }

    fn text_g(s: &[u8]) -> IResult<&[u8], &str> {
        let (s, res) = sized('t', ',')(s)?;
        Ok((
            s,
            std::str::from_utf8(res).map_err(|_| nom::Err::Failure((s, ErrorKind::Char)))?,
        ))
    }

    fn binary<'a>() -> impl Fn(&'a [u8]) -> IResult<&'a [u8], T> {
        map(binary_g(), |b| T::Binary(b.to_owned()))
    }

    fn binary_g() -> impl Fn(&[u8]) -> IResult<&[u8], &[u8]> {
        sized('b', ',')
    }

    fn list_t(s: &[u8]) -> IResult<&[u8], Vec<T>> {
        list_g(t_t)(s)
    }

    /// Wrap the inner parser of an `many0`/`fold_many0`, so that the parser
    /// is not called when the `s` is empty already, preventing it from
    /// returning `Incomplete` on streaming parsing.
    fn inner_no_empty_string<'a, P, O>(inner: P) -> impl Fn(&'a [u8]) -> IResult<&'a [u8], O>
    where
        O: Clone,
        P: Fn(&'a [u8]) -> IResult<&'a [u8], O>,
    {
        move |s: &'a [u8]| {
            if s.is_empty() {
                // This is a bit hacky, `many0` considers the inside done
                // when a parser returns `Err::Error`, ignoring the actual error content
                Err(nom::Err::Error((s, nom::error::ErrorKind::Many0)))
            } else {
                inner(s)
            }
        }
    }

    fn list_g<'a, P, O>(inner: P) -> impl Fn(&'a [u8]) -> IResult<&'a [u8], Vec<O>>
    where
        O: Clone,
        P: Fn(&'a [u8]) -> IResult<&'a [u8], O>,
    {
        map_parser(
            sized('[', ']'),
            nom::multi::many0(inner_no_empty_string(inner)),
        )
    }

    fn record_t<'a>(s: &'a [u8]) -> IResult<&'a [u8], IndexMap<String, T>> {
        let (s, r) = record_g(t_t)(s)?;
        Ok((
            s,
            r.into_iter()
                .map(|(k, v)| (k.to_string(), v))
                .collect::<IndexMap<_, _>>(),
        ))
    }

    fn record_g<'a, P, O>(inner: P) -> impl Fn(&'a [u8]) -> IResult<&'a [u8], IndexMap<&'a str, O>>
    where
        O: Clone,
        P: Fn(&'a [u8]) -> IResult<&'a [u8], O>,
    {
        move |s: &'a [u8]| {
            let (s, map) = map_parser(
                sized('{', '}'),
                nom::multi::fold_many0(
                    inner_no_empty_string(tag_g(&inner)),
                    IndexMap::new(),
                    |mut acc: IndexMap<_, _>, Tag { tag, mut val }| {
                        // ignore earlier tags with the same name
                        // according to netencode spec
                        let _ = acc.insert(tag, *val);
                        acc
                    },
                ),
            )(s)?;
            if map.is_empty() {
                // records must not be empty, according to the spec
                Err(nom::Err::Failure((s, nom::error::ErrorKind::Many1)))
            } else {
                Ok((s, map))
            }
        }
    }

    pub fn u_u(s: &[u8]) -> IResult<&[u8], U> {
        alt((
            map(text_g, U::Text),
            map(binary_g(), U::Binary),
            map(unit_t, |()| U::Unit),
            map(tag_g(u_u), |t| U::Sum(t)),
            map(list_g(u_u), U::List),
            map(record_g(u_u), U::Record),
            map(uint_t("n"), |u| U::N(u)),
            map(int_t("i"), |u| U::I(u)),
        ))(s)
    }

    pub fn t_t(s: &[u8]) -> IResult<&[u8], T> {
        alt((
            text,
            binary(),
            map(unit_t, |_| T::Unit),
            map(tag_t, |t| T::Sum(t)),
            map(list_t, |l| T::List(l)),
            map(record_t, |p| T::Record(p)),
            map(uint_t("n"), |u| T::N(u)),
            map(int_t("i"), |u| T::I(u)),
        ))(s)
    }

    #[cfg(test)]
    mod tests {
        use super::*;

        #[test]
        fn test_parse_unit_t() {
            assert_eq!(unit_t("u,".as_bytes()), Ok(("".as_bytes(), ())));
        }

        #[test]
        fn test_parse_bool_t() {
            assert_eq!(bool_t()("<5:false|u,".as_bytes()), Ok(("".as_bytes(), false)));
            assert_eq!(bool_t()("<4:true|u,".as_bytes()), Ok(("".as_bytes(), true)));
        }

        #[test]
        fn test_parse_usize_t() {
            assert_eq!(usize_t("32foo".as_bytes()), Ok(("foo".as_bytes(), 32)));
        }

        #[test]
        fn test_parse_int_t() {
            assert_eq!(
                uint_t::<u64>("n")("n:42,abc".as_bytes()),
                Ok(("abc".as_bytes(), 42))
            );
            assert_eq!(
                int_t::<i64>("i")("i:-23,abc".as_bytes()),
                Ok(("abc".as_bytes(), -23))
            );
            assert_eq!(
                int_t::<i64>("i")("i:0,:abc".as_bytes()),
                Ok((":abc".as_bytes(), 0))
            );
            assert_eq!(
                uint_t::<u64>("n")("n:09,".as_bytes()),
                Ok(("".as_bytes(), 9))
            );
            // assert_eq!(
            //     length("c"),
            //     Err(nom::Err::Error(("c", nom::error::ErrorKind::Digit)))
            // );
            // assert_eq!(
            //     length(":"),
            //     Err(nom::Err::Error((":", nom::error::ErrorKind::Digit)))
            // );
        }

        #[test]
        fn test_parse_text() {
            assert_eq!(
                text("t5:hello,".as_bytes()),
                Ok(("".as_bytes(), T::Text("hello".to_owned()))),
                "{}",
                r"t5:hello,"
            );
            assert_eq!(
                text("t4:fo".as_bytes()),
                // The content of the text should be 4 long
                Err(nom::Err::Incomplete(nom::Needed::Size(4))),
                "{}",
                r"t4:fo,"
            );
            assert_eq!(
                text("t9:今日は,".as_bytes()),
                Ok(("".as_bytes(), T::Text("今日は".to_owned()))),
                "{}",
                r"t9:今日は,"
            );
        }

        #[test]
        fn test_parse_binary() {
            assert_eq!(
                binary()("b5:hello,".as_bytes()),
                Ok(("".as_bytes(), T::Binary(Vec::from("hello".to_owned())))),
                "{}",
                r"b5:hello,"
            );
            assert_eq!(
                binary()("b4:fo".as_bytes()),
                // The content of the byte should be 4 long
                Err(nom::Err::Incomplete(nom::Needed::Size(4))),
                "{}",
                r"b4:fo,"
            );
            assert_eq!(
                binary()("b4:foob".as_bytes()),
                // The content is 4 bytes now, but the finishing , is missing
                Err(nom::Err::Incomplete(nom::Needed::Size(1))),
                "{}",
                r"b4:fo,"
            );
            assert_eq!(
                binary()("b9:今日は,".as_bytes()),
                Ok(("".as_bytes(), T::Binary(Vec::from("今日は".as_bytes())))),
                "{}",
                r"b9:今日は,"
            );
        }

        #[test]
        fn test_list() {
            assert_eq!(
                list_t("[0:]".as_bytes()),
                Ok(("".as_bytes(), vec![])),
                "{}",
                r"[0:]"
            );
            assert_eq!(
                list_t("[6:u,u,u,]".as_bytes()),
                Ok(("".as_bytes(), vec![T::Unit, T::Unit, T::Unit,])),
                "{}",
                r"[6:u,u,u,]"
            );
            assert_eq!(
                list_t("[15:u,[7:t3:foo,]u,]".as_bytes()),
                Ok((
                    "".as_bytes(),
                    vec![T::Unit, T::List(vec![T::Text("foo".to_owned())]), T::Unit,]
                )),
                "{}",
                r"[15:u,[7:t3:foo,]u,]"
            );
        }

        #[test]
        fn test_record() {
            assert_eq!(
                record_t("{21:<1:a|u,<1:b|u,<1:c|u,}".as_bytes()),
                Ok((
                    "".as_bytes(),
                    vec![
                        ("a".to_owned(), T::Unit),
                        ("b".to_owned(), T::Unit),
                        ("c".to_owned(), T::Unit),
                    ]
                    .into_iter()
                    .collect::<IndexMap<String, T>>()
                )),
                "{}",
                r"{21:<1:a|u,<1:b|u,<1:c|u,}"
            );
            // duplicated keys are ignored (first is taken)
            assert_eq!(
                record_t("{24:<1:a|u,<1:b|u,<1:a|i:-1,}".as_bytes()),
                Ok((
                    "".as_bytes(),
                    vec![("a".to_owned(), T::I(-1)), ("b".to_owned(), T::Unit),]
                        .into_iter()
                        .collect::<IndexMap<_, _>>()
                )),
                "{}",
                r"{24:<1:a|u,<1:b|u,<1:a|i:-1,}"
            );
            // empty records are not allowed
            assert_eq!(
                record_t("{0:}".as_bytes()),
                Err(nom::Err::Failure((
                    "".as_bytes(),
                    nom::error::ErrorKind::Many1
                ))),
                "{}",
                r"{0:}"
            );
        }

        #[test]
        fn test_parse() {
            assert_eq!(
                t_t("n:255,".as_bytes()),
                Ok(("".as_bytes(), T::N(255))),
                "{}",
                r"n:255,"
            );
            assert_eq!(
                t_t("t6:halloo,".as_bytes()),
                Ok(("".as_bytes(), T::Text("halloo".to_owned()))),
                "{}",
                r"t6:halloo,"
            );
            assert_eq!(
                t_t("<3:foo|t6:halloo,".as_bytes()),
                Ok((
                    "".as_bytes(),
                    T::Sum(Tag {
                        tag: "foo".to_owned(),
                        val: Box::new(T::Text("halloo".to_owned()))
                    })
                )),
                "{}",
                r"<3:foo|t6:halloo,"
            );
            // { a: Unit
            // , foo: List <A: Unit | B: List i> }
            assert_eq!(
                t_t("{56:<3:foo|[37:<1:A|u,<1:A|<4:true|u,<1:B|[6:i:127,]]<1:a|u,}".as_bytes()),
                Ok((
                    "".as_bytes(),
                    T::Record(
                        vec![
                            ("a".to_owned(), T::Unit),
                            (
                                "foo".to_owned(),
                                T::List(vec![
                                    T::Sum(Tag {
                                        tag: "A".to_owned(),
                                        val: Box::new(T::Unit)
                                    }),
                                    T::Sum(Tag {
                                        tag: "A".to_owned(),
                                        val: Box::new(T::Sum(Tag {
                                            tag: "true".to_owned(),
                                            val: Box::new(T::Unit)
                                        }))
                                    }),
                                    T::Sum(Tag {
                                        tag: "B".to_owned(),
                                        val: Box::new(T::List(vec![T::I(127)]))
                                    }),
                                ])
                            )
                        ]
                        .into_iter()
                        .collect::<IndexMap<String, T>>()
                    )
                )),
                "{}",
                r"{56:<3:foo|[37:<1:A|u,<1:A|<4:true|u,<1:B|[6:i:127,]]<1:a|u,}"
            );
        }
    }
}

pub mod dec {
    use super::*;
    use indexmap::IndexMap;

    pub struct DecodeError(pub String);

    pub trait Decoder<'a> {
        type A;
        fn dec(&self, u: U<'a>) -> Result<Self::A, DecodeError>;
    }

    /// Any netencode, as `T`.
    #[derive(Clone, Copy)]
    pub struct AnyT;
    /// Any netencode, as `U`.
    #[derive(Clone, Copy)]
    pub struct AnyU;

    impl<'a> Decoder<'a> for AnyT {
        type A = T;
        fn dec(&self, u: U<'a>) -> Result<Self::A, DecodeError> {
            Ok(u.to_t())
        }
    }

    impl<'a> Decoder<'a> for AnyU {
        type A = U<'a>;
        fn dec(&self, u: U<'a>) -> Result<Self::A, DecodeError> {
            Ok(u)
        }
    }

    /// A text
    #[derive(Clone, Copy)]
    pub struct Text;

    /// A bytestring
    // TODO: rename to Bytes
    #[derive(Clone, Copy)]
    pub struct Binary;

    impl<'a> Decoder<'a> for Text {
        type A = &'a str;
        fn dec(&self, u: U<'a>) -> Result<Self::A, DecodeError> {
            match u {
                U::Text(t) => Ok(t),
                other => Err(DecodeError(format!("Cannot decode {:?} into Text", other))),
            }
        }
    }

    impl<'a> Decoder<'a> for Binary {
        type A = &'a [u8];
        fn dec(&self, u: U<'a>) -> Result<Self::A, DecodeError> {
            match u {
                U::Binary(b) => Ok(b),
                other => Err(DecodeError(format!(
                    "Cannot decode {:?} into Binary",
                    other
                ))),
            }
        }
    }

    /// Any scalar, converted to bytes.
    #[derive(Clone, Copy)]
    pub struct ScalarAsBytes;

    impl<'a> Decoder<'a> for ScalarAsBytes {
        type A = Vec<u8>;
        fn dec(&self, u: U<'a>) -> Result<Self::A, DecodeError> {
            match u {
                U::N(u) => Ok(format!("{}", u).into_bytes()),
                U::I(i) => Ok(format!("{}", i).into_bytes()),
                U::Text(t) => Ok(t.as_bytes().to_owned()),
                U::Binary(b) => Ok(b.to_owned()),
                o => Err(DecodeError(format!("Cannot decode {:?} into scalar", o))),
            }
        }
    }

    /// A map of Ts (TODO: rename to map)
    #[derive(Clone, Copy)]
    pub struct Record<T>(pub T);

    impl<'a, Inner> Decoder<'a> for Record<Inner>
    where
        Inner: Decoder<'a>,
    {
        type A = IndexMap<&'a str, Inner::A>;
        fn dec(&self, u: U<'a>) -> Result<Self::A, DecodeError> {
            match u {
                U::Record(map) => map
                    .into_iter()
                    .map(|(k, v)| self.0.dec(v).map(|v2| (k, v2)))
                    .collect::<Result<Self::A, _>>(),
                o => Err(DecodeError(format!("Cannot decode {:?} into record", o))),
            }
        }
    }

    /// Assume a record and project out the field with the given name and type.
    #[derive(Clone, Copy)]
    pub struct RecordDot<'a, T> {
        pub field: &'a str,
        pub inner: T,
    }

    impl<'a, Inner> Decoder<'a> for RecordDot<'_, Inner>
    where
        Inner: Decoder<'a> + Clone,
    {
        type A = Inner::A;
        fn dec(&self, u: U<'a>) -> Result<Self::A, DecodeError> {
            match Record(self.inner.clone()).dec(u) {
                Ok(mut map) => match map.shift_remove(self.field) {
                    Some(inner) => Ok(inner),
                    None => Err(DecodeError(format!(
                        "Cannot find `{}` in record map",
                        self.field
                    ))),
                },
                Err(err) => Err(err),
            }
        }
    }

    /// Equals one of the listed `A`s exactly, after decoding.
    #[derive(Clone)]
    pub struct OneOf<T, A> {
        pub inner: T,
        pub list: Vec<A>,
    }

    impl<'a, Inner> Decoder<'a> for OneOf<Inner, Inner::A>
    where
        Inner: Decoder<'a>,
        Inner::A: Display + Debug + PartialEq,
    {
        type A = Inner::A;
        fn dec(&self, u: U<'a>) -> Result<Self::A, DecodeError> {
            match self.inner.dec(u) {
                Ok(inner) => match self.list.iter().any(|x| x.eq(&inner)) {
                    true => Ok(inner),
                    false => Err(DecodeError(format!(
                        "{} is not one of {:?}",
                        inner, self.list
                    ))),
                },
                Err(err) => Err(err),
            }
        }
    }

    /// Try decoding as `T`.
    #[derive(Clone)]
    pub struct Try<T>(pub T);

    impl<'a, Inner> Decoder<'a> for Try<Inner>
    where
        Inner: Decoder<'a>,
    {
        type A = Option<Inner::A>;
        fn dec(&self, u: U<'a>) -> Result<Self::A, DecodeError> {
            match self.0.dec(u) {
                Ok(inner) => Ok(Some(inner)),
                Err(err) => Ok(None),
            }
        }
    }
}
