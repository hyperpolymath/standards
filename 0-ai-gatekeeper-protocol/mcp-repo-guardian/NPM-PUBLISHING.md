# NPM Publishing Guide

**SPDX-License-Identifier: PMPL-1.0-or-later**

This document describes how to publish `@hyperpolymath/mcp-repo-guardian` to npm.

## Prerequisites

1. **npm account** - Create at https://www.npmjs.com/signup
2. **npm login** - Run `npm login` and authenticate
3. **Organization access** - Join `@hyperpolymath` organization on npm (or create it)

## Pre-Publication Checklist

- [x] Package.json properly configured
  - [x] Name: `@hyperpolymath/mcp-repo-guardian`
  - [x] Version: `0.1.0`
  - [x] Author: Jonathan D.A. Jewell <jonathan.jewell@open.ac.uk>
  - [x] License: PMPL-1.0-or-later
  - [x] Repository, bugs, homepage URLs
  - [x] Keywords for discoverability
- [x] .npmignore configured (excludes source, dev files)
- [x] TypeScript build working (`npm run build`)
- [x] README with installation instructions
- [x] LICENSE file present
- [x] All changes committed and pushed to GitHub

## Publishing Commands

### First-Time Publication

```bash
cd ~/Documents/hyperpolymath-repos/mcp-repo-guardian

# Ensure you're logged in
npm login

# Verify package contents (dry run)
npm pack --dry-run

# Publish as public package
npm publish --access public
```

### Subsequent Releases

```bash
# Update version (choose one)
npm version patch  # 0.1.0 → 0.1.1
npm version minor  # 0.1.0 → 0.2.0
npm version major  # 0.1.0 → 1.0.0

# Push version tag
git push && git push --tags

# Publish
npm publish --access public
```

## Post-Publication

1. Verify package on npm: https://www.npmjs.com/package/@hyperpolymath/mcp-repo-guardian
2. Test installation: `npm install -g @hyperpolymath/mcp-repo-guardian`
3. Test execution: `mcp-repo-guardian --help` (should work globally)
4. Update documentation repos with npm availability

## Package Structure

What gets published (via .npmignore):

```
@hyperpolymath/mcp-repo-guardian/
├── dist/              # Compiled JavaScript + type definitions
│   ├── index.js
│   ├── index.d.ts
│   ├── manifest.js
│   ├── manifest.d.ts
│   ├── session.js
│   ├── session.d.ts
│   ├── guards.js
│   ├── guards.d.ts
│   ├── types.js
│   └── types.d.ts
├── README.md          # Installation and usage
├── LICENSE            # PMPL-1.0-or-later full text
└── package.json       # Metadata
```

What gets excluded (via .npmignore):

- Source files (src/)
- TypeScript config (tsconfig.json)
- Development files (.github/, .editorconfig, etc.)
- Documentation (docs/, examples/, contractiles/, etc.)
- Repository management (.bot_directives/, .machines_readable/, SCM files)

## Installation Verification

After publishing, test with:

```bash
# Global installation
npm install -g @hyperpolymath/mcp-repo-guardian

# Verify binary works
which mcp-repo-guardian
mcp-repo-guardian --version

# Test in MCP configuration
# Edit ~/.claude/settings.json and add:
{
  "mcpServers": {
    "repo-guardian": {
      "command": "mcp-repo-guardian",
      "env": {
        "REPOS_PATH": "/path/to/repos"
      }
    }
  }
}
```

## Troubleshooting

### "You do not have permission to publish"

You need to be added to the `@hyperpolymath` organization on npm:

```bash
# Ask organization owner to run:
npm org:add hyperpolymath <your-username>
```

Or publish under your own username first, then transfer to organization.

### "Package name already exists"

If someone else registered `@hyperpolymath/mcp-repo-guardian`, choose alternative:

- `@hyperpolymath/repo-guardian-mcp`
- `mcp-ai-gatekeeper`
- Contact npm support to claim abandoned package

### Build errors

```bash
# Clean and rebuild
rm -rf dist/ node_modules/
npm install
npm run build
```

## Version History

- **0.1.0** (2026-02-07) - Initial release
  - MCP server with hard enforcement
  - Session management and attestation
  - 4 tools: get_manifest, acknowledge_manifest, read_file, list_directory

## Related

- Main documentation: https://github.com/hyperpolymath/0-ai-gatekeeper-protocol
- GitHub repository: https://github.com/hyperpolymath/mcp-repo-guardian
- npm package: https://www.npmjs.com/package/@hyperpolymath/mcp-repo-guardian (after publishing)
