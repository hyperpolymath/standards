# AUR Submission Guide for k9-svc

## Prerequisites

1. An AUR account at https://aur.archlinux.org/
2. SSH key added to your AUR account
3. `git` installed

## Submission Steps

### 1. Clone the AUR package base

```bash
git clone ssh://aur@aur.archlinux.org/k9-svc.git aur-k9-svc
cd aur-k9-svc
```

If this fails (package doesn't exist yet), it will create an empty repo.

### 2. Copy package files

```bash
cp /path/to/k9-svc/packaging/aur/PKGBUILD .
cp /path/to/k9-svc/packaging/aur/.SRCINFO .
```

### 3. Verify the PKGBUILD

```bash
# Check PKGBUILD syntax
namcap PKGBUILD

# Test build locally
makepkg -si
```

### 4. Commit and push

```bash
git add PKGBUILD .SRCINFO
git commit -m "Initial upload: k9-svc 1.0.0"
git push origin master
```

## Updating the Package

When releasing a new version:

1. Update `pkgver` in PKGBUILD
2. Update `sha256sums` (run `updpkgsums`)
3. Regenerate .SRCINFO: `makepkg --printsrcinfo > .SRCINFO`
4. Commit and push

## Package Details

- **Package name**: k9-svc
- **Version**: 1.0.0
- **Dependencies**: nickel, just, openssl
- **Optional**: podman, asciidoctor
- **License**: AGPL-3.0-or-later

## Testing

After installation, verify with:

```bash
k9 status
k9 typecheck
k9 test
```

## Maintainer

hyperpolymath <hyperpolymath@users.noreply.github.com>
