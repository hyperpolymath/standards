// SPDX-License-Identifier: MPL-2.0
//! `uuid-v8` — command-line front end to `estate_uuid_v8`, with the same
//! interface and exit codes as `scripts/uuid-v8.js`:
//!
//!   uuid-v8 t                   # profile T: time-ordered
//!   uuid-v8 c <domain> <name>   # profile C: SHA-256(domain ":" name)
//!   uuid-v8 deed <domain:name>  # profile C from a deed #u8 body

use std::process::ExitCode;

use estate_uuid_v8::{from_deed_body, profile_c, profile_t};

fn main() -> ExitCode {
    let args: Vec<String> = std::env::args().skip(1).collect();
    let args: Vec<&str> = args.iter().map(String::as_str).collect();
    let result = match args.as_slice() {
        ["t"] => Ok(profile_t()),
        ["c", domain, name] => profile_c(domain, name),
        ["deed", body] => from_deed_body(body),
        _ => {
            eprintln!("usage: uuid-v8 t | c <domain> <name> | deed <domain:name>");
            return ExitCode::from(2);
        }
    };
    match result {
        Ok(id) => {
            println!("{id}");
            ExitCode::SUCCESS
        }
        Err(e) => {
            eprintln!("uuid-v8: {e}");
            ExitCode::from(1)
        }
    }
}
