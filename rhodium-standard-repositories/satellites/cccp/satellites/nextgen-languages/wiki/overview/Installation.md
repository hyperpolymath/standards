# Installation Guide

## Prerequisites

- **Rust** (1.70+) - Required for most language implementations
- **Git** - For cloning repositories
- **Node.js** (18+) - For web-based tools and playground

### Optional
- **Racket** - For betlang
- **Elixir** - For Phronesis
- **LLVM** (15+) - For native compilation

## Quick Install

### Using the Installer Script

```bash
curl -fsSL https://nextgen-languages.dev/install.sh | bash
```

This installs:
- `nextgen` CLI tool
- All language interpreters
- Package manager
- Build tools

### Manual Installation

#### 1. Clone the Hub Repository

```bash
git clone https://github.com/hyperpolymath/nextgen-languages.git
cd nextgen-languages
git submodule update --init --recursive
```

#### 2. Build Tools

```bash
# Build the CLI
cargo build --release -p nextgen-cli

# Add to PATH
export PATH="$PATH:$(pwd)/target/release"
```

#### 3. Verify Installation

```bash
nextgen --version
nextgen languages list
```

## Installing Individual Languages

### Solo (Recommended First Language)

```bash
nextgen install solo
# or manually:
git clone https://github.com/hyperpolymath/solo.git
cd solo
cargo build --release
```

### Duet

```bash
nextgen install duet
```

### Ensemble

```bash
nextgen install ensemble
```

### betlang

Requires Racket:
```bash
# Install Racket first
# macOS: brew install racket
# Ubuntu: sudo apt install racket

nextgen install betlang
```

### julia-the-viper

```bash
nextgen install julia-the-viper
```

### Phronesis

Requires Elixir:
```bash
# Install Elixir first
# macOS: brew install elixir
# Ubuntu: sudo apt install elixir

nextgen install phronesis
```

### Eclexia

```bash
nextgen install eclexia
```

### Specialized Languages

```bash
nextgen install oblibeny
nextgen install anvomidav
nextgen install wokelang
```

## IDE Setup

### Visual Studio Code

1. Install the NextGen Languages extension:
   ```
   code --install-extension nextgen-languages.vscode-nextgen
   ```

2. Or search for "NextGen Languages" in the Extensions marketplace

### Neovim

Using lazy.nvim:
```lua
{
  "nextgen-languages/nvim-nextgen",
  config = function()
    require("nextgen").setup()
  end
}
```

### Emacs

```elisp
(use-package nextgen-mode
  :straight (:host github :repo "nextgen-languages/emacs-nextgen"))
```

## Platform-Specific Notes

### macOS

```bash
# Install dependencies via Homebrew
brew install rust llvm racket elixir
```

### Linux (Ubuntu/Debian)

```bash
sudo apt update
sudo apt install build-essential curl git
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
```

### Windows

1. Install [Rust](https://rustup.rs/)
2. Install [Git for Windows](https://git-scm.com/download/win)
3. Use PowerShell or WSL2

WSL2 recommended for best experience.

## Docker

```dockerfile
FROM rust:latest
RUN cargo install nextgen-cli
```

Or use our official image:
```bash
docker pull nextgen-languages/nextgen:latest
docker run -it nextgen-languages/nextgen solo repl
```

## Verifying Your Installation

Run the test suite:
```bash
nextgen doctor
```

This checks:
- ✓ All required dependencies
- ✓ Language interpreters
- ✓ LSP servers
- ✓ Package manager connectivity

## Troubleshooting

### "Command not found: nextgen"

Ensure the installation directory is in your PATH:
```bash
echo 'export PATH="$HOME/.nextgen/bin:$PATH"' >> ~/.bashrc
source ~/.bashrc
```

### Build Failures

Update Rust:
```bash
rustup update stable
```

### Permission Errors

Don't use `sudo` with Cargo. If you have permission issues:
```bash
sudo chown -R $USER ~/.cargo ~/.nextgen
```

## Next Steps

- [[Your First Program]]
- [[Choosing a Language]]
- [[IDE Setup]]
