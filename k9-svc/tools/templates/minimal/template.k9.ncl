# SPDX-License-Identifier: PMPL-1.0-or-later
# K9 Contractile Template: Minimal
# Security Level: kennel (read-only, safe operations)

{
  metadata = {
    name = "{{NAME}}",
    version = "1.0.0",
    author = "{{AUTHOR}}",
    security_level = "{{SECURITY_LEVEL}}",
    created = "{{CREATED_DATE}}",
  },

  must = {
    check = ''
      #!/usr/bin/env bash
      # Validation: Check if prerequisites are met

      echo "Checking prerequisites for {{NAME}}..."

      # Example: Check if directory exists
      if [ -d "$HOME/.{{NAME}}/" ]; then
        echo "✓ Configuration directory exists"
        exit 0
      else
        echo "✗ Configuration directory missing"
        exit 1
      fi
    '',
  },

  just = {
    recipes = {
      setup = {
        description = "Set up {{NAME}}",
        dependencies = [],
        commands = [
          "mkdir -p $HOME/.{{NAME}}/",
          "echo 'Setup complete for {{NAME}}!'",
        ],
      },

      clean = {
        description = "Clean up {{NAME}}",
        dependencies = [],
        commands = [
          "rm -rf $HOME/.{{NAME}}/",
          "echo 'Cleanup complete for {{NAME}}!'",
        ],
      },

      status = {
        description = "Check {{NAME}} status",
        dependencies = [],
        commands = [
          "ls -la $HOME/.{{NAME}}/ 2>/dev/null || echo 'Not configured'",
        ],
      },
    },
  },

  nickel = {
    config = {
      app_name = "{{NAME}}",
      enabled = true,
      debug_mode = false,
    },
  },
}
