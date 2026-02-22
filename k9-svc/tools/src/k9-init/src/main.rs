// SPDX-License-Identifier: PMPL-1.0-or-later
//! k9-init: Scaffold new K9 contractiles from templates

use anyhow::{Context, Result};
use clap::Parser;
use dialoguer::{Input, Select};
use std::fs;
use std::path::PathBuf;
use tera::{Context as TeraContext, Tera};

#[derive(Parser, Debug)]
#[command(name = "k9-init")]
#[command(about = "Scaffold new K9 contractiles from templates", long_about = None)]
struct Args {
    /// Template name (web-server, build, deploy, dev-env, etc.)
    #[arg(short, long)]
    template: Option<String>,

    /// Contractile name
    #[arg(short, long)]
    name: Option<String>,

    /// Security level (kennel, yard, hunt)
    #[arg(short, long)]
    security: Option<String>,

    /// Output directory
    #[arg(short, long, default_value = ".")]
    output: PathBuf,

    /// Non-interactive mode (use all defaults/provided values)
    #[arg(long)]
    non_interactive: bool,
}

#[derive(Debug)]
struct ContractileConfig {
    name: String,
    security_level: SecurityLevel,
    template: Template,
    author: String,
}

#[derive(Debug, Clone, Copy)]
enum SecurityLevel {
    Kennel,
    Yard,
    Hunt,
}

impl SecurityLevel {
    fn as_str(&self) -> &str {
        match self {
            SecurityLevel::Kennel => "kennel",
            SecurityLevel::Yard => "yard",
            SecurityLevel::Hunt => "hunt",
        }
    }

    fn description(&self) -> &str {
        match self {
            SecurityLevel::Kennel => "Read-only, safe operations (ls, cat, stat)",
            SecurityLevel::Yard => "File writes, git ops, package installs",
            SecurityLevel::Hunt => "Full system access (requires signature)",
        }
    }
}

#[derive(Debug, Clone)]
enum Template {
    WebServer(String),    // nginx, caddy, apache
    Build(String),        // rust, node, go, python
    Deploy(String),       // docker, kubernetes, terraform
    DevEnv(String),       // minimal, full-stack, data-science
    CiCd,
    Backup,
    Monitoring,
    Minimal,
}

impl Template {
    fn name(&self) -> String {
        match self {
            Template::WebServer(variant) => format!("web-server/{}", variant),
            Template::Build(variant) => format!("build/{}", variant),
            Template::Deploy(variant) => format!("deploy/{}", variant),
            Template::DevEnv(variant) => format!("dev-env/{}", variant),
            Template::CiCd => "ci-cd".to_string(),
            Template::Backup => "backup".to_string(),
            Template::Monitoring => "monitoring".to_string(),
            Template::Minimal => "minimal".to_string(),
        }
    }
}

fn main() -> Result<()> {
    let args = Args::parse();

    println!("╔══════════════════════════════════════════════════════════╗");
    println!("║         K9 Init - Scaffold K9 Contractiles              ║");
    println!("╚══════════════════════════════════════════════════════════╝");
    println!();

    let config = if args.non_interactive {
        build_config_non_interactive(&args)?
    } else {
        build_config_interactive(&args)?
    };

    scaffold_contractile(&config, &args.output)?;

    println!();
    println!("✅ K9 contractile scaffolded successfully!");
    println!();
    println!("Next steps:");
    println!("  1. cd {}", config.name);
    println!("  2. Review {}.k9.ncl", config.name);
    println!("  3. Validate: k9-validate {}.k9.ncl", config.name);
    println!("  4. Test: just --list");
    println!("  5. Sign: k9-sign sign {}.k9.ncl", config.name);
    println!();

    Ok(())
}

fn build_config_interactive(args: &Args) -> Result<ContractileConfig> {
    // Contractile name
    let name = if let Some(name) = &args.name {
        name.clone()
    } else {
        Input::<String>::new()
            .with_prompt("Contractile name")
            .default("my-contractile".to_string())
            .interact_text()?
    };

    // Security level
    let security_level = if let Some(level) = &args.security {
        match level.as_str() {
            "kennel" => SecurityLevel::Kennel,
            "yard" => SecurityLevel::Yard,
            "hunt" => SecurityLevel::Hunt,
            _ => anyhow::bail!("Invalid security level: {}", level),
        }
    } else {
        let levels = vec![
            format!("kennel - {}", SecurityLevel::Kennel.description()),
            format!("yard   - {}", SecurityLevel::Yard.description()),
            format!("hunt   - {}", SecurityLevel::Hunt.description()),
        ];
        let selection = Select::new()
            .with_prompt("Security level")
            .items(&levels)
            .default(0)
            .interact()?;

        match selection {
            0 => SecurityLevel::Kennel,
            1 => SecurityLevel::Yard,
            2 => SecurityLevel::Hunt,
            _ => unreachable!(),
        }
    };

    // Template
    let template = if let Some(template_name) = &args.template {
        parse_template(template_name)?
    } else {
        let templates = vec![
            "web-server/nginx - Deploy nginx web server",
            "web-server/caddy - Deploy Caddy web server",
            "build/rust - Rust build automation",
            "build/node - Node.js build automation",
            "deploy/docker - Docker deployment",
            "deploy/kubernetes - Kubernetes deployment",
            "dev-env/minimal - Minimal development environment",
            "dev-env/full-stack - Full-stack development environment",
            "ci-cd - CI/CD pipeline",
            "minimal - Minimal K9 contractile",
        ];
        let selection = Select::new()
            .with_prompt("Template")
            .items(&templates)
            .default(0)
            .interact()?;

        match selection {
            0 => Template::WebServer("nginx".to_string()),
            1 => Template::WebServer("caddy".to_string()),
            2 => Template::Build("rust".to_string()),
            3 => Template::Build("node".to_string()),
            4 => Template::Deploy("docker".to_string()),
            5 => Template::Deploy("kubernetes".to_string()),
            6 => Template::DevEnv("minimal".to_string()),
            7 => Template::DevEnv("full-stack".to_string()),
            8 => Template::CiCd,
            9 => Template::Minimal,
            _ => unreachable!(),
        }
    };

    // Author
    let author = Input::<String>::new()
        .with_prompt("Author")
        .default("Jonathan D.A. Jewell <jonathan.jewell@open.ac.uk>".to_string())
        .interact_text()?;

    Ok(ContractileConfig {
        name,
        security_level,
        template,
        author,
    })
}

fn build_config_non_interactive(args: &Args) -> Result<ContractileConfig> {
    let name = args.name.clone().unwrap_or_else(|| "my-contractile".to_string());
    let security_level = match args.security.as_deref() {
        Some("kennel") => SecurityLevel::Kennel,
        Some("yard") => SecurityLevel::Yard,
        Some("hunt") => SecurityLevel::Hunt,
        Some(other) => anyhow::bail!("Invalid security level: {}", other),
        None => SecurityLevel::Kennel, // Default
    };
    let template = if let Some(template_name) = &args.template {
        parse_template(template_name)?
    } else {
        Template::Minimal // Default
    };
    let author = "Jonathan D.A. Jewell <jonathan.jewell@open.ac.uk>".to_string();

    Ok(ContractileConfig {
        name,
        security_level,
        template,
        author,
    })
}

fn parse_template(name: &str) -> Result<Template> {
    let parts: Vec<&str> = name.split('/').collect();
    match parts.as_slice() {
        ["web-server", variant] => Ok(Template::WebServer(variant.to_string())),
        ["build", variant] => Ok(Template::Build(variant.to_string())),
        ["deploy", variant] => Ok(Template::Deploy(variant.to_string())),
        ["dev-env", variant] => Ok(Template::DevEnv(variant.to_string())),
        ["ci-cd"] => Ok(Template::CiCd),
        ["backup"] => Ok(Template::Backup),
        ["monitoring"] => Ok(Template::Monitoring),
        ["minimal"] => Ok(Template::Minimal),
        _ => anyhow::bail!("Unknown template: {}", name),
    }
}

fn scaffold_contractile(config: &ContractileConfig, output_dir: &PathBuf) -> Result<()> {
    let contractile_dir = output_dir.join(&config.name);
    fs::create_dir_all(&contractile_dir)
        .context("Failed to create contractile directory")?;

    println!("📁 Creating directory: {}", contractile_dir.display());

    // Generate contractile file
    let contractile_content = generate_contractile(config)?;
    let contractile_path = contractile_dir.join(format!("{}.k9.ncl", config.name));
    fs::write(&contractile_path, contractile_content)
        .context("Failed to write contractile file")?;
    println!("✓ Created: {}.k9.ncl", config.name);

    // Generate README
    let readme_content = generate_readme(config)?;
    let readme_path = contractile_dir.join("README.md");
    fs::write(&readme_path, readme_content)
        .context("Failed to write README")?;
    println!("✓ Created: README.md");

    // Generate .gitignore
    let gitignore_content = generate_gitignore();
    let gitignore_path = contractile_dir.join(".gitignore");
    fs::write(&gitignore_path, gitignore_content)
        .context("Failed to write .gitignore")?;
    println!("✓ Created: .gitignore");

    Ok(())
}

fn generate_contractile(config: &ContractileConfig) -> Result<String> {
    let template_content = match &config.template {
        Template::Minimal => generate_minimal_template(config),
        Template::WebServer(variant) => generate_webserver_template(config, variant),
        Template::Build(variant) => generate_build_template(config, variant),
        Template::Deploy(variant) => generate_deploy_template(config, variant),
        Template::DevEnv(variant) => generate_devenv_template(config, variant),
        Template::CiCd => generate_cicd_template(config),
        Template::Backup => generate_backup_template(config),
        Template::Monitoring => generate_monitoring_template(config),
    };

    Ok(template_content)
}

fn generate_minimal_template(config: &ContractileConfig) -> String {
    format!(r#"# SPDX-License-Identifier: PMPL-1.0-or-later
# K9 Contractile: {}
# Security Level: {}
# Author: {}

{{
  metadata = {{
    name = "{}",
    version = "1.0.0",
    author = "{}",
    security_level = "{}",
    created = "{}",
  }},

  must = {{
    check = ''
      #!/usr/bin/env bash
      # Validation: Check if prerequisites are met

      echo "Checking prerequisites..."

      # Example: Check if directory exists
      if [ -d "$HOME/.{}/" ]; then
        echo "✓ Configuration directory exists"
        exit 0
      else
        echo "✗ Configuration directory missing"
        exit 1
      fi
    '',
  }},

  just = {{
    recipes = {{
      setup = {{
        description = "Set up {}",
        dependencies = [],
        commands = [
          "mkdir -p $HOME/.{}/",
          "echo 'Setup complete!'",
        ],
      }},

      clean = {{
        description = "Clean up {}",
        dependencies = [],
        commands = [
          "rm -rf $HOME/.{}/",
          "echo 'Cleanup complete!'",
        ]},
      }},
    }},
  }},

  nickel = {{
    config = {{
      app_name = "{}",
      enabled = true,
    }},
  }},
}}
"#,
        config.name,
        config.security_level.as_str(),
        config.author,
        config.name,
        config.author,
        config.security_level.as_str(),
        chrono::Local::now().format("%Y-%m-%d"),
        config.name,
        config.name,
        config.name,
        config.name,
        config.name,
        config.name,
    )
}

fn generate_webserver_template(config: &ContractileConfig, variant: &str) -> String {
    // Similar structure but with web server specific commands
    format!(r#"# SPDX-License-Identifier: PMPL-1.0-or-later
# K9 Contractile: {} ({} Web Server)
# Security Level: {}
# Author: {}

{{
  metadata = {{
    name = "{}",
    version = "1.0.0",
    author = "{}",
    security_level = "{}",
    webserver = "{}",
  }},

  must = {{
    check = ''
      #!/usr/bin/env bash
      # Check if {} is installed

      if command -v {} &> /dev/null; then
        echo "✓ {} is installed"
        exit 0
      else
        echo "✗ {} not found"
        exit 1
      fi
    '',
  }},

  just = {{
    recipes = {{
      install = {{
        description = "Install {}",
        commands = [
          "sudo apt-get update",
          "sudo apt-get install -y {}",
        ],
      }},

      start = {{
        description = "Start {} server",
        commands = [
          "sudo systemctl start {}",
          "sudo systemctl status {}",
        ],
      }},

      stop = {{
        description = "Stop {} server",
        commands = [
          "sudo systemctl stop {}",
        ],
      }},
    }},
  }},

  nickel = {{
    config = {{
      port = 80,
      ssl_enabled = false,
      document_root = "/var/www/html",
    }},
  }},
}}
"#,
        config.name,
        variant,
        config.security_level.as_str(),
        config.author,
        config.name,
        config.author,
        config.security_level.as_str(),
        variant,
        variant,
        variant,
        variant,
        variant,
        variant,
        variant,
        variant,
        variant,
        variant,
        variant,
        variant,
    )
}

fn generate_build_template(config: &ContractileConfig, variant: &str) -> String {
    format!(r#"# K9 Contractile: {} ({} Build)
# Security Level: {}

{{
  metadata = {{
    name = "{}",
    build_system = "{}",
    security_level = "{}",
  }},

  must = {{
    check = ''
      #!/usr/bin/env bash
      command -v {} &> /dev/null || exit 1
    '',
  }},

  just = {{
    recipes = {{
      build = {{
        description = "Build project",
        commands = ["Build commands for {}..."],
      }},
      test = {{
        description = "Run tests",
        commands = ["Test commands..."],
      }},
    }},
  }},

  nickel = {{
    config = {{
      build_type = "release",
    }},
  }},
}}
"#,
        config.name, variant, config.security_level.as_str(),
        config.name, variant, config.security_level.as_str(),
        variant, variant
    )
}

fn generate_deploy_template(_config: &ContractileConfig, _variant: &str) -> String {
    "# Deploy template...".to_string()
}

fn generate_devenv_template(_config: &ContractileConfig, _variant: &str) -> String {
    "# Dev env template...".to_string()
}

fn generate_cicd_template(_config: &ContractileConfig) -> String {
    "# CI/CD template...".to_string()
}

fn generate_backup_template(_config: &ContractileConfig) -> String {
    "# Backup template...".to_string()
}

fn generate_monitoring_template(_config: &ContractileConfig) -> String {
    "# Monitoring template...".to_string()
}

fn generate_readme(config: &ContractileConfig) -> Result<String> {
    Ok(format!(r#"# {}

K9 Contractile for {}.

## Security Level

**{}** - {}

## Usage

```bash
# Validate
k9-validate {}.k9.ncl

# Execute
just --list
just setup

# Sign
k9-sign sign {}.k9.ncl
```

## Template

Generated from: `{}`

## Author

{}

## License

PMPL-1.0-or-later
"#,
        config.name,
        config.name,
        config.security_level.as_str(),
        config.security_level.description(),
        config.name,
        config.name,
        config.template.name(),
        config.author,
    ))
}

fn generate_gitignore() -> String {
    r#"# K9 Generated Files
.k9/
*.sig

# Editor
.vscode/
.idea/
*.swp

# OS
.DS_Store
Thumbs.db
"#.to_string()
}
