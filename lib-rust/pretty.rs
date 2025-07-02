extern crate netencode;

use netencode::{Tag, T, U};

pub enum Pretty {
    Single {
        r#type: char,
        length: String,
        val: String,
        trailer: char,
    },
    Tag {
        r#type: char,
        length: String,
        key: String,
        inner: char,
        val: Box<Pretty>,
    },
    Multi {
        r#type: char,
        length: String,
        vals: Vec<Pretty>,
        trailer: char,
    },
    // Multi-line content (for long text or binary hexdumps)
    MultiLine {
        r#type: char,
        length_info: String,  // e.g., "[127]" for truncated content
        lines: Vec<String>,
        trailer: char,
    },
}

impl Pretty {
    const TEXT_THRESHOLD: usize = 40;
    const BINARY_SINGLE_LINE_THRESHOLD: usize = 16;
    const HEXDUMP_MAX_ROWS: usize = 6;

    fn format_text(s: &str) -> Pretty {
        if s.len() <= Self::TEXT_THRESHOLD {
            // Short text - show complete
            Pretty::Single {
                r#type: 't',
                length: String::new(),
                val: s.to_string(),
                trailer: ',',
            }
        } else {
            // Long text - truncate with length indicator
            let truncated = format!("{}...", &s[..Self::TEXT_THRESHOLD]);
            Pretty::Single {
                r#type: 't',
                length: format!("{}", s.len()),
                val: truncated,
                trailer: ',',
            }
        }
    }

    fn format_binary(data: &[u8]) -> Pretty {
        // Check if it's valid UTF-8
        if let Ok(utf8_str) = std::str::from_utf8(data) {
            // Still binary format (b prefix) but display as text content
            if utf8_str.len() <= Self::TEXT_THRESHOLD {
                Pretty::Single {
                    r#type: 'b',
                    length: format!("{}", data.len()),
                    val: utf8_str.to_string(),
                    trailer: ',',
                }
            } else {
                // Long UTF-8 binary - truncate with length indicator
                let truncated = format!("{}...", &utf8_str[..Self::TEXT_THRESHOLD]);
                Pretty::Single {
                    r#type: 'b',
                    length: format!("{}", data.len()),
                    val: truncated,
                    trailer: ',',
                }
            }
        } else if data.len() <= Self::BINARY_SINGLE_LINE_THRESHOLD {
            // Short binary - single line hex
            let hex = data.iter()
                .map(|b| format!("{:02x}", b))
                .collect::<Vec<_>>()
                .join("");
            Pretty::Single {
                r#type: 'b',
                length: format!("{}", data.len()),
                val: hex,
                trailer: ',',
            }
        } else {
            // Long binary - multi-line hexdump
            let lines = Self::format_hexdump(data);
            Pretty::MultiLine {
                r#type: 'b',
                length_info: format!("{}", data.len()),
                lines,
                trailer: ',',
            }
        }
    }

    fn format_hexdump(data: &[u8]) -> Vec<String> {
        let mut lines = Vec::new();
        let rows_to_show = std::cmp::min(Self::HEXDUMP_MAX_ROWS, (data.len() + 15) / 16);
        
        for row in 0..rows_to_show {
            let offset = row * 16;
            let row_data = &data[offset..std::cmp::min(offset + 16, data.len())];
            
            let mut line = String::new();
            
            // Offset column
            line.push_str(&format!("{:08x}  ", offset));
            
            // Hex columns (8 + space + 8)
            for (i, byte) in row_data.iter().enumerate() {
                if i == 8 { line.push(' '); }
                line.push_str(&format!("{:02x} ", byte));
            }
            
            // Pad if last row is short
            let padding = (16 - row_data.len()) * 3 + if row_data.len() <= 8 { 1 } else { 0 };
            line.push_str(&" ".repeat(padding));
            
            // ASCII column
            line.push_str(" |");
            for byte in row_data {
                let c = if byte.is_ascii_graphic() || *byte == b' ' { 
                    *byte as char 
                } else { 
                    '.' 
                };
                line.push(c);
            }
            line.push('|');
            
            lines.push(line);
        }
        
        // Show truncation if needed
        if data.len() > rows_to_show * 16 {
            lines.push(format!("... ({} more bytes)", data.len() - rows_to_show * 16));
        }
        
        lines
    }

    pub fn from_u<'a>(u: U<'a>) -> Pretty {
        match u {
            U::Unit => Self::scalar('u', ""),
            U::N(n) => Self::scalar('n', n),
            U::I(i) => Self::scalar('i', i),
            U::Text(s) => Self::format_text(s),
            U::Binary(s) => Self::format_binary(s),
            U::Sum(Tag { tag, val }) => Self::pretty_tag(tag, Self::from_u(*val)),
            U::Record(m) => Pretty::Multi {
                r#type: '{',
                // TODO: we are losing the size here, should we recompute it? Keep it?
                length: String::from(""),
                vals: m
                    .into_iter()
                    .map(|(k, v)| Self::pretty_tag(k, Self::from_u(v)))
                    .collect(),
                trailer: '}',
            },
            U::List(l) => Pretty::Multi {
                r#type: '[',
                // TODO: we are losing the size here, should we recompute it? Keep it?
                length: String::from(""),
                vals: l.into_iter().map(|v| Self::from_u(v)).collect(),
                trailer: ']',
            },
        }
    }

    fn scalar<D>(r#type: char, d: D) -> Pretty
    where
        D: std::fmt::Display,
    {
        Pretty::Single {
            r#type,
            length: String::new(),
            val: format!("{}", d),
            trailer: ',',
        }
    }

    fn pretty_tag(tag: &str, val: Pretty) -> Pretty {
        Pretty::Tag {
            r#type: '<',
            length: format!("{}:", tag.len()),
            key: tag.to_string(),
            inner: '|',
            val: Box::new(val),
        }
    }

    pub fn print_multiline<W>(&self, mut w: &mut W) -> std::io::Result<()>
    where
        W: std::io::Write,
    {
        Self::go(&mut w, self, 0, true);
        write!(w, "\n")
    }

    fn go<W>(mut w: &mut W, p: &Pretty, depth: usize, is_newline: bool) -> std::io::Result<()>
    where
        W: std::io::Write,
    {
        const full: usize = 4;
        const half: usize = 2;
        let i = &vec![b' '; depth * full];
        let iandhalf = &vec![b' '; depth * full + half];
        let (i, iandhalf) = unsafe {
            (
                std::str::from_utf8_unchecked(i),
                std::str::from_utf8_unchecked(iandhalf),
            )
        };
        if is_newline {
            write!(&mut w, "{}", i);
        }
        match p {
            Pretty::Single {
                r#type,
                length,
                val,
                trailer,
            } => {
                if length.is_empty() {
                    write!(&mut w, "{} {}{}", r#type, val, trailer)
                } else {
                    write!(&mut w, "{}{} {}{}", r#type, length, val, trailer)
                }
            },
            Pretty::Tag {
                r#type,
                length,
                key,
                inner,
                val,
            } => {
                write!(&mut w, "{} {} {}", r#type, key, inner)?;
                Self::go::<W>(&mut w, val, depth, false)
            }
            // if the length is 0 or 1, we print on one line,
            // only if there's more than one element we split the resulting value.
            // we never break lines on arbitrary column sizes, since that is just silly.
            Pretty::Multi {
                r#type,
                length,
                vals,
                trailer,
            } => match vals.len() {
                0 => write!(&mut w, "{} {}", r#type, trailer),
                1 => {
                    write!(&mut w, "{} ", r#type);
                    Self::go::<W>(&mut w, &vals[0], depth, false)?;
                    write!(&mut w, "{}", trailer)
                }
                more => {
                    write!(&mut w, "\n{}{}\n", iandhalf, r#type)?;
                    for v in vals {
                        Self::go::<W>(&mut w, v, depth + 1, true)?;
                        write!(&mut w, "\n")?;
                    }
                    write!(&mut w, "{}{}", iandhalf, trailer)
                }
            },
            // Handle multi-line content (hexdumps)
            Pretty::MultiLine {
                r#type,
                length_info,
                lines,
                trailer,
            } => {
                write!(&mut w, "{}{}\n", r#type, length_info)?;
                for line in lines {
                    write!(&mut w, "{}{}\n", i, line)?;
                }
                write!(&mut w, "{}", i)?; // Indent for trailer
                Ok(())
            }
        }
    }
}