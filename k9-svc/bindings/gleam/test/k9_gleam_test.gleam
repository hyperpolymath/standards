// SPDX-License-Identifier: MPL-2.0
// (PMPL-1.0-or-later preferred; MPL-2.0 required for Hex.pm)

import gleam/dict
import k9_gleam/parser
import k9_gleam/renderer
import k9_gleam/types.{
  Component, Hunt, Kennel, Pedigree, SecurityPolicy, Yard,
}
import gleeunit

pub fn main() -> Nil {
  gleeunit.main()
}

// ---------------------------------------------------------------------------
// Parser tests
// ---------------------------------------------------------------------------

pub fn parse_empty_input_test() {
  let result = parser.parse("")
  assert result == Error(parser.EmptyInput)
}

pub fn parse_minimal_component_test() {
  let input =
    "pedigree:\n  name: hello-k9\n  version: 1.0.0\n  description: A greeting\n\nsecurity:\n  level: kennel"
  let assert Ok(component) = parser.parse(input)
  assert component.pedigree.name == "hello-k9"
  assert component.pedigree.version == "1.0.0"
  assert component.security.level == Kennel
  assert component.security.allow_network == False
}

pub fn parse_security_levels_test() {
  let assert Ok(Kennel) = parser.parse_security_level("kennel")
  let assert Ok(Yard) = parser.parse_security_level("Yard")
  let assert Ok(Hunt) = parser.parse_security_level("HUNT")
  let assert Error(parser.UnknownSecurityLevel("invalid")) =
    parser.parse_security_level("invalid")
}

pub fn parse_full_component_test() {
  let input =
    "pedigree:
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

tags: parser, k9, gleam"

  let assert Ok(component) = parser.parse(input)
  assert component.pedigree.name == "my-component"
  assert component.pedigree.author == Ok("Jonathan D.A. Jewell")
  assert component.pedigree.license == Ok("MPL-2.0")
  assert component.security.level == Yard
  assert component.security.allow_network == True
  let assert Ok(target) = component.target
  assert target.os == Ok("Linux")
  assert target.requires_podman == True
  assert target.memory == Ok("512M")
  let assert Ok(recipes) = component.recipes
  assert recipes.install == Ok("just install")
  assert recipes.validate == Ok("just check")
}

pub fn parse_missing_pedigree_name_test() {
  let input = "pedigree:\n  version: 1.0.0\n\nsecurity:\n  level: kennel"
  let assert Error(parser.MissingField("pedigree.name")) = parser.parse(input)
}

// ---------------------------------------------------------------------------
// Renderer tests
// ---------------------------------------------------------------------------

pub fn render_security_level_test() {
  assert renderer.render_security_level(Kennel) == "kennel"
  assert renderer.render_security_level(Yard) == "yard"
  assert renderer.render_security_level(Hunt) == "hunt"
}

pub fn render_minimal_component_test() {
  let component =
    Component(
      pedigree: Pedigree(
        name: "test-k9",
        version: "0.1.0",
        description: "Test component",
        author: Error(Nil),
        license: Error(Nil),
      ),
      security: SecurityPolicy(
        level: Kennel,
        allow_network: False,
        allow_fs_write: False,
        allow_subprocess: False,
      ),
      target: Error(Nil),
      recipes: Error(Nil),
      validation: Error(Nil),
      content: dict.new(),
      tags: [],
    )
  let output = renderer.render(component)
  let assert True = output |> contains("pedigree:")
  let assert True = output |> contains("name: test-k9")
  let assert True = output |> contains("level: kennel")
}

pub fn render_roundtrip_test() {
  let input =
    "pedigree:\n  name: roundtrip\n  version: 1.0.0\n  description: Roundtrip test\n\nsecurity:\n  level: yard\n  allow-network: true\n  allow-fs-write: false\n  allow-subprocess: false"
  let assert Ok(component) = parser.parse(input)
  let output = renderer.render(component)
  let assert Ok(component2) = parser.parse(output)
  assert component.pedigree.name == component2.pedigree.name
  assert component.security.level == component2.security.level
  assert component.security.allow_network == component2.security.allow_network
}

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

import gleam/string

fn contains(haystack: String, needle: String) -> Bool {
  string.contains(haystack, needle)
}
