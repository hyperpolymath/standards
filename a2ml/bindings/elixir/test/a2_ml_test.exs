# SPDX-License-Identifier: MPL-2.0
# (PMPL-1.0-or-later preferred; MPL-2.0 required for Hex.pm)

defmodule A2MLTest do
  use ExUnit.Case, async: true

  alias A2ML.Types.{Attestation, Directive, Document, Manifest, TrustLevel}

  # ---------------------------------------------------------------------------
  # Parser tests
  # ---------------------------------------------------------------------------

  test "parse empty input returns error" do
    assert A2ML.parse("") == {:error, :empty_input}
    assert A2ML.parse("   ") == {:error, :empty_input}
  end

  test "parse title only" do
    assert {:ok, %Document{title: "Hello A2ML"}} = A2ML.parse("# Hello A2ML")
  end

  test "parse heading levels" do
    assert {:ok, doc} = A2ML.parse("## Second Level\n\n### Third Level")
    assert [{:heading, 2, _}, {:heading, 3, _}] = doc.blocks
  end

  test "parse paragraph" do
    assert {:ok, doc} = A2ML.parse("This is a simple paragraph.")
    assert [{:paragraph, [{:text, "This is a simple paragraph."}]}] = doc.blocks
  end

  test "parse directive" do
    assert {:ok, doc} = A2ML.parse("@version 1.0")

    assert [{:directive, %Directive{name: "version", value: "1.0"}}] =
             doc.blocks

    assert [%Directive{name: "version", value: "1.0"}] = doc.directives
  end

  test "parse trust levels" do
    assert {:ok, :unverified} = TrustLevel.from_string("unverified")
    assert {:ok, :automated} = TrustLevel.from_string("Automated")
    assert {:ok, :reviewed} = TrustLevel.from_string("REVIEWED")
    assert {:ok, :verified} = TrustLevel.from_string("verified")
    assert {:error, :unknown_trust_level} = TrustLevel.from_string("unknown")
  end

  test "parse attestation" do
    input = "!attest\n  identity: Alice\n  role: reviewer\n  trust-level: reviewed"
    assert {:ok, doc} = A2ML.parse(input)
    assert [{:attestation, %Attestation{identity: "Alice", role: "reviewer", trust_level: :reviewed}}] = doc.blocks
  end

  test "parse full document" do
    input = """
    # Test Doc

    @version 2.0

    A paragraph here.

    !attest
      identity: Bob
      role: author
      trust-level: verified
    """

    assert {:ok, doc} = A2ML.parse(input)
    assert doc.title == "Test Doc"
    assert [%Directive{name: "version"}] = doc.directives
    assert [%Attestation{identity: "Bob"}] = doc.attestations
  end

  # ---------------------------------------------------------------------------
  # Renderer tests
  # ---------------------------------------------------------------------------

  test "render trust level strings" do
    assert TrustLevel.to_string(:unverified) == "unverified"
    assert TrustLevel.to_string(:automated) == "automated"
    assert TrustLevel.to_string(:reviewed) == "reviewed"
    assert TrustLevel.to_string(:verified) == "verified"
  end

  test "render attestation" do
    a = %Attestation{
      identity: "Claude",
      role: "agent",
      trust_level: :automated,
      timestamp: "2026-03-16T12:00:00Z",
      note: nil
    }

    rendered = A2ML.Renderer.render_attestation(a)

    assert rendered ==
             "!attest\n  identity: Claude\n  role: agent\n  trust-level: automated\n  timestamp: 2026-03-16T12:00:00Z"
  end

  test "render roundtrip" do
    input = "# Round Trip\n\n@version 1.0\n\nHello, world!"
    assert {:ok, doc} = A2ML.parse(input)
    output = A2ML.render(doc)
    assert {:ok, doc2} = A2ML.parse(output)
    assert doc.title == doc2.title
  end

  # ---------------------------------------------------------------------------
  # Manifest tests
  # ---------------------------------------------------------------------------

  test "extract manifest" do
    input = "# Manifest Test\n\n@version 3.0\n\n@author Alice"
    assert {:ok, doc} = A2ML.parse(input)
    manifest = Manifest.from_document(doc)
    assert manifest.version == "3.0"
    assert manifest.title == "Manifest Test"
  end

  # ---------------------------------------------------------------------------
  # Trust level comparison
  # ---------------------------------------------------------------------------

  test "trust level comparison" do
    assert TrustLevel.compare(:unverified, :verified) == :lt
    assert TrustLevel.compare(:verified, :unverified) == :gt
    assert TrustLevel.compare(:reviewed, :reviewed) == :eq
  end
end
