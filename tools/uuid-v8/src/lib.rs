// SPDX-License-Identifier: MPL-2.0
//! ESTATE-UUID-V8 generators (ADR-008,
//! `docs/decisions/ADR-008-uuid-v8-estate-standard.adoc`): the shared Rust
//! implementation of both profiles, matching `scripts/uuid-v8.js`.
//!
//! * **Profile T** — time-ordered. A library v7 with its version nibble set to
//!   `8`, the only bit operation ADR-008 permits. Within one process the
//!   `uuid` crate's shared v7 context keeps successive ids strictly
//!   increasing, and the nibble flip preserves that order.
//! * **Profile C** — content-derived. Bytes 0..15 of SHA-256(`domain ":" name`)
//!   with the version nibble set to `8` and the variant bits to `10`. `domain`
//!   is a registered, repository-unique, non-empty printable-ASCII string
//!   without `:`, so the first `:` of the preimage always ends it. `name` is
//!   unconstrained. Profile C ids are reproducible and not secret.
//!
//! A v8 value does not say which profile produced it; the field that holds it
//! declares `profile T` or `profile C` in its type, schema or contract.

use std::fmt;

use sha2::{Digest, Sha256};
use uuid::Uuid;

/// Why an identifier could not be produced.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Error {
    /// A profile C domain that is empty, contains `:`, or has a byte outside
    /// printable ASCII (`0x21..=0x7e`).
    InvalidDomain(String),
    /// A deed `#u8` body with no `:` separating domain and name.
    DeedBodyWithoutColon(String),
    /// [`from_v7`] was given an id whose version is not 7.
    NotVersion7(Uuid),
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Error::InvalidDomain(d) => write!(
                f,
                "profile C domain must be non-empty printable ASCII without \":\" (got {d:?})"
            ),
            Error::DeedBodyWithoutColon(b) => write!(f, "#u8 body has no \":\" (got {b:?})"),
            Error::NotVersion7(id) => write!(f, "expected a version 7 UUID (got {id})"),
        }
    }
}

impl std::error::Error for Error {}

/// A fresh profile T id: a library v7 with its version nibble set to 8.
pub fn profile_t() -> Uuid {
    let mut bytes = *Uuid::now_v7().as_bytes();
    set_version_8(&mut bytes);
    Uuid::from_bytes(bytes)
}

/// The profile T alias of an existing v7 id (ADR-008 phase P2: "every v7 read
/// is aliased to profile T by the nibble flip").
pub fn from_v7(id: Uuid) -> Result<Uuid, Error> {
    if id.get_version_num() != 7 {
        return Err(Error::NotVersion7(id));
    }
    let mut bytes = *id.as_bytes();
    set_version_8(&mut bytes);
    Ok(Uuid::from_bytes(bytes))
}

/// The profile C id of (`domain`, `name`).
pub fn profile_c(domain: &str, name: &str) -> Result<Uuid, Error> {
    profile_c_bytes(domain, name.as_bytes())
}

/// The profile C id of (`domain`, `name`) for a name given as bytes, such as
/// a value's JCS (RFC 8785) canonical form.
pub fn profile_c_bytes(domain: &str, name: &[u8]) -> Result<Uuid, Error> {
    validate_domain(domain)?;
    let digest = Sha256::new()
        .chain_update(domain.as_bytes())
        .chain_update(b":")
        .chain_update(name)
        .finalize();
    let mut bytes = [0u8; 16];
    bytes.copy_from_slice(&digest[..16]);
    set_version_8(&mut bytes);
    bytes[8] = (bytes[8] & 0x3f) | 0x80; // variant 10
    Ok(Uuid::from_bytes(bytes))
}

/// The profile C id named by a deed `#u8` body (`"domain:name"`). The body
/// splits on its first `:`.
pub fn from_deed_body(body: &str) -> Result<Uuid, Error> {
    match body.split_once(':') {
        Some((domain, name)) => profile_c(domain, name),
        None => Err(Error::DeedBodyWithoutColon(body.to_owned())),
    }
}

/// Check that `domain` is a valid profile C domain.
pub fn validate_domain(domain: &str) -> Result<(), Error> {
    let ok = !domain.is_empty()
        && domain
            .bytes()
            .all(|b| (0x21..=0x7e).contains(&b) && b != b':');
    if ok {
        Ok(())
    } else {
        Err(Error::InvalidDomain(domain.to_owned()))
    }
}

fn set_version_8(bytes: &mut [u8; 16]) {
    bytes[6] = (bytes[6] & 0x0f) | 0x80;
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::time::{SystemTime, UNIX_EPOCH};
    use uuid::Variant;

    /// Expected values from the independent oracle in
    /// `scripts/tests/bun/uuid-v8-gen-test.sh` (sha256sum and shell nibble
    /// arithmetic), not from this code.
    const ORACLE: &[(&str, &str, &str)] = &[
        (
            "gv-clade-index",
            "github.com/hyperpolymath/standards",
            "b32d6be4-73ba-873e-9d61-e57f43c75cd1",
        ),
        (
            "berrywiki-import",
            "Main Page",
            "c2be2559-9f9e-8131-aa58-1b0158cc2645",
        ),
        ("d", "", "394e0f7e-2f80-8af2-b682-28464ce30d58"),
        ("d", "a:b c", "ca23a717-72b1-8d99-94c5-e32a4fda7acd"),
        ("echidna-goal", "é✓", "939052b9-3381-8187-9199-1960e0a527a4"),
    ];

    #[test]
    fn profile_c_matches_the_independent_oracle() {
        for (domain, name, want) in ORACLE {
            let got = profile_c(domain, name).unwrap();
            assert_eq!(got.to_string(), *want, "{domain}:{name}");
            assert_eq!(got.get_version_num(), 8);
            assert_eq!(got.get_variant(), Variant::RFC4122);
            assert_eq!(profile_c_bytes(domain, name.as_bytes()).unwrap(), got);
        }
    }

    #[test]
    fn deed_body_splits_on_the_first_colon() {
        assert_eq!(
            from_deed_body("d:a:b c").unwrap().to_string(),
            "ca23a717-72b1-8d99-94c5-e32a4fda7acd"
        );
        assert!(matches!(
            from_deed_body("no-colon"),
            Err(Error::DeedBodyWithoutColon(_))
        ));
    }

    #[test]
    fn bad_domains_are_rejected() {
        for domain in ["", "a:b", "a b", "tab\there", "é"] {
            assert!(
                matches!(profile_c(domain, "x"), Err(Error::InvalidDomain(_))),
                "{domain:?} must be rejected"
            );
        }
        assert!(validate_domain("!~").is_ok(), "0x21 and 0x7e are allowed");
    }

    #[test]
    fn profile_t_is_version_8_variant_10_and_now() {
        let now_ms = || {
            SystemTime::now()
                .duration_since(UNIX_EPOCH)
                .unwrap()
                .as_millis() as u64
        };
        let before = now_ms() - 1000;
        let id = profile_t();
        let after = now_ms() + 1000;
        assert_eq!(id.get_version_num(), 8);
        assert_eq!(id.get_variant(), Variant::RFC4122);
        let b = id.as_bytes();
        let ms = b[..6]
            .iter()
            .fold(0u64, |acc, &x| (acc << 8) | u64::from(x));
        assert!((before..=after).contains(&ms), "timestamp {ms} is not now");
    }

    #[test]
    fn profile_t_is_strictly_increasing_in_process() {
        let ids: Vec<Uuid> = (0..10_000).map(|_| profile_t()).collect();
        assert!(ids.windows(2).all(|w| w[0] < w[1]));
    }

    #[test]
    fn from_v7_flips_only_the_version_nibble() {
        let v7 = Uuid::now_v7();
        let t = from_v7(v7).unwrap();
        assert_eq!(t.get_version_num(), 8);
        let (a, b) = (v7.as_bytes(), t.as_bytes());
        for i in 0..16 {
            if i == 6 {
                assert_eq!(a[i] & 0x0f, b[i] & 0x0f);
            } else {
                assert_eq!(a[i], b[i]);
            }
        }
        let v4 = Uuid::from_bytes([
            0x11, 0x22, 0x33, 0x44, 0x55, 0x66, 0x47, 0x77, 0x88, 0, 0, 0, 0, 0, 0, 0,
        ]);
        assert!(matches!(from_v7(v4), Err(Error::NotVersion7(_))));
    }
}
