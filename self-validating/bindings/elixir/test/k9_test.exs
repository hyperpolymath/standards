# SPDX-License-Identifier: MPL-2.0
# (PMPL-1.0-or-later preferred; MPL-2.0 required for Hex.pm)

defmodule K9Test do
  use ExUnit.Case, async: true

  alias K9.Types.{Component, Pedigree, SecurityLevel, SecurityPolicy}

  # ---------------------------------------------------------------------------
  # Parser tests
  # ---------------------------------------------------------------------------

  test "parse empty input returns error" do
    assert K9.parse("") == {:error, :empty_input}
    assert K9.parse("   ") == {:error, :empty_input}
  end

  test "parse minimal component" do
    input =
      "pedigree:\n  name: hello-k9\n  version: 1.0.0\n  description: A greeting\n\nsecurity:\n  level: kennel"

    assert {:ok, %Component{} = c} = K9.parse(input)
    assert c.pedigree.name == "hello-k9"
    assert c.pedigree.version == "1.0.0"
    assert c.security.level == :kennel
    assert c.security.allow_network == false
  end

  test "parse security levels" do
    assert {:ok, :kennel} = SecurityLevel.from_string("kennel")
    assert {:ok, :yard} = SecurityLevel.from_string("Yard")
    assert {:ok, :hunt} = SecurityLevel.from_string("HUNT")
    assert {:error, :unknown_security_level} = SecurityLevel.from_string("invalid")
  end

  test "parse full component" do
    input = """
    pedigree:
      name: my-component
      version: 2.0.0
      description: A full component
      author: Jonathan D.A. Jewell
      license: MPL-2.0

    security:
      level: yard
      allow-network: true
      allow-fs-write: false
      allow-subprocess: false

    target:
      os: Linux
      edge: false
      requires-podman: true
      memory: 512M

    recipes:
      install: just install
      validate: just check

    tags: parser, k9, elixir
    """

    assert {:ok, %Component{} = c} = K9.parse(input)
    assert c.pedigree.name == "my-component"
    assert c.pedigree.author == "Jonathan D.A. Jewell"
    assert c.pedigree.license == "MPL-2.0"
    assert c.security.level == :yard
    assert c.security.allow_network == true
    assert c.target.os == "Linux"
    assert c.target.requires_podman == true
    assert c.target.memory == "512M"
    assert c.recipes.install == "just install"
    assert c.recipes.validate == "just check"
    assert c.tags == ["parser", "k9", "elixir"]
  end

  test "parse missing pedigree name" do
    input = "pedigree:\n  version: 1.0.0\n\nsecurity:\n  level: kennel"
    assert {:error, {:missing_field, "pedigree.name"}} = K9.parse(input)
  end

  # ---------------------------------------------------------------------------
  # Renderer tests
  # ---------------------------------------------------------------------------

  test "render security level strings" do
    assert SecurityLevel.to_string(:kennel) == "kennel"
    assert SecurityLevel.to_string(:yard) == "yard"
    assert SecurityLevel.to_string(:hunt) == "hunt"
  end

  test "render minimal component" do
    component = %Component{
      pedigree: %Pedigree{
        name: "test-k9",
        version: "0.1.0",
        description: "Test component"
      },
      security: %SecurityPolicy{
        level: :kennel,
        allow_network: false,
        allow_fs_write: false,
        allow_subprocess: false
      }
    }

    output = K9.render(component)
    assert output =~ "pedigree:"
    assert output =~ "name: test-k9"
    assert output =~ "level: kennel"
  end

  test "render roundtrip" do
    input =
      "pedigree:\n  name: roundtrip\n  version: 1.0.0\n  description: Roundtrip test\n\nsecurity:\n  level: yard\n  allow-network: true\n  allow-fs-write: false\n  allow-subprocess: false"

    assert {:ok, c1} = K9.parse(input)
    output = K9.render(c1)
    assert {:ok, c2} = K9.parse(output)
    assert c1.pedigree.name == c2.pedigree.name
    assert c1.security.level == c2.security.level
    assert c1.security.allow_network == c2.security.allow_network
  end

  # ---------------------------------------------------------------------------
  # Security level comparison
  # ---------------------------------------------------------------------------

  test "security level comparison" do
    assert SecurityLevel.compare(:kennel, :hunt) == :lt
    assert SecurityLevel.compare(:hunt, :kennel) == :gt
    assert SecurityLevel.compare(:yard, :yard) == :eq
  end
end
