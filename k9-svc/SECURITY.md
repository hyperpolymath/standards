# Security Policy

## Supported Versions

| Version | Supported          |
| ------- | ------------------ |
| 1.x     | :white_check_mark: |

## Reporting a Vulnerability

If you discover a security vulnerability in K9 SVC, please report it by:

1. **DO NOT** create a public GitHub issue
2. Email security concerns to the maintainer privately
3. Include a detailed description of the vulnerability
4. Provide steps to reproduce if possible

## Security Model

K9 implements a tiered security model called "The Leash":

- **Kennel**: Pure data, no execution
- **Yard**: Nickel validation only, no I/O
- **Hunt**: Full execution, requires cryptographic handshake

For details, see [SPEC.adoc](SPEC.adoc#security-model).

## Security Considerations

Since K9 files can contain executable logic:

1. Never run `'Hunt` level components from untrusted sources
2. Always validate components in `'Yard` mode first
3. Review Just recipes before granting execution permission
4. Use the cryptographic handshake for production deployments
