# SPDX-License-Identifier: MPL-2.0
# (PMPL-1.0-or-later preferred; MPL-2.0 required for Hex.pm)

defmodule A2ML.Types do
  @moduledoc """
  Core data types for A2ML documents.

  An A2ML document consists of a sequence of `Block` elements, each of
  which may contain `Inline` content.  `Directive` blocks provide
  machine-readable metadata, and `Attestation` records capture the
  provenance chain for AI-generated or human-reviewed content.
  """
end

defmodule A2ML.Types.Document do
  @moduledoc """
  A complete A2ML document, containing metadata and a sequence of blocks.
  """

  @type t :: %__MODULE__{
          title: String.t() | nil,
          directives: [A2ML.Types.Directive.t()],
          blocks: [A2ML.Types.Block.t()],
          attestations: [A2ML.Types.Attestation.t()]
        }

  defstruct title: nil,
            directives: [],
            blocks: [],
            attestations: []
end

defmodule A2ML.Types.Block do
  @moduledoc """
  A block-level element in an A2ML document.

  Blocks are separated by blank lines in the source text.
  """

  @type t ::
          {:heading, non_neg_integer(), [A2ML.Types.Inline.t()]}
          | {:paragraph, [A2ML.Types.Inline.t()]}
          | {:code_block, String.t() | nil, String.t()}
          | {:directive, A2ML.Types.Directive.t()}
          | {:attestation, A2ML.Types.Attestation.t()}
          | :thematic_break
          | {:block_quote, [t()]}
          | {:list_block, boolean(), [[t()]]}
end

defmodule A2ML.Types.Inline do
  @moduledoc """
  An inline-level element within a block.
  """

  @type t ::
          {:text, String.t()}
          | {:emphasis, [t()]}
          | {:strong, [t()]}
          | {:code, String.t()}
          | {:link, [t()], String.t()}
end

defmodule A2ML.Types.Directive do
  @moduledoc """
  A machine-readable directive that provides metadata or instructions.

  Directives begin with `@` in the source text, e.g.
  `@version 1.0` or `@require trust-level:high`.
  """

  @type t :: %__MODULE__{
          name: String.t(),
          value: String.t(),
          attributes: [{String.t(), String.t()}]
        }

  defstruct name: "",
            value: "",
            attributes: []
end

defmodule A2ML.Types.Attestation do
  @moduledoc """
  An attestation record capturing who produced or reviewed content.

  Attestation blocks start with `!attest` and record the identity,
  role, trust level, and optional timestamp of an author or reviewer.
  """

  @type t :: %__MODULE__{
          identity: String.t(),
          role: String.t(),
          trust_level: A2ML.Types.TrustLevel.t(),
          timestamp: String.t() | nil,
          note: String.t() | nil
        }

  defstruct identity: "",
            role: "",
            trust_level: :unverified,
            timestamp: nil,
            note: nil
end

defmodule A2ML.Types.TrustLevel do
  @moduledoc """
  The degree of trust associated with an attestation.

  Trust levels form a simple ordered scale:
  - `:unverified` — Content with no verification or review.
  - `:automated` — Content reviewed by an automated tool or linter.
  - `:reviewed` — Content reviewed by a human.
  - `:verified` — Content that has been formally verified or proven.
  """

  @type t :: :unverified | :automated | :reviewed | :verified

  @doc """
  Parse a trust level from its canonical string representation.

  Returns `{:ok, level}` or `{:error, :unknown_trust_level}`.
  """
  @spec from_string(String.t()) :: {:ok, t()} | {:error, :unknown_trust_level}
  def from_string(s) do
    case String.downcase(String.trim(s)) do
      "unverified" -> {:ok, :unverified}
      "automated" -> {:ok, :automated}
      "reviewed" -> {:ok, :reviewed}
      "verified" -> {:ok, :verified}
      _ -> {:error, :unknown_trust_level}
    end
  end

  @doc """
  Return the canonical string representation of a trust level.
  """
  @spec to_string(t()) :: String.t()
  def to_string(:unverified), do: "unverified"
  def to_string(:automated), do: "automated"
  def to_string(:reviewed), do: "reviewed"
  def to_string(:verified), do: "verified"

  @doc """
  Compare two trust levels.  Returns `:lt`, `:eq`, or `:gt`.
  """
  @spec compare(t(), t()) :: :lt | :eq | :gt
  def compare(a, b), do: compare_int(level_to_int(a), level_to_int(b))

  defp level_to_int(:unverified), do: 0
  defp level_to_int(:automated), do: 1
  defp level_to_int(:reviewed), do: 2
  defp level_to_int(:verified), do: 3

  defp compare_int(a, b) when a < b, do: :lt
  defp compare_int(a, b) when a > b, do: :gt
  defp compare_int(_, _), do: :eq
end

defmodule A2ML.Types.Manifest do
  @moduledoc """
  A high-level manifest extracted from a parsed A2ML document.

  Collects the directives and attestations into a single structure
  for convenient programmatic access.
  """

  @type t :: %__MODULE__{
          version: String.t() | nil,
          title: String.t() | nil,
          directives: [A2ML.Types.Directive.t()],
          attestations: [A2ML.Types.Attestation.t()]
        }

  defstruct version: nil,
            title: nil,
            directives: [],
            attestations: []

  @doc """
  Extract a manifest from a parsed Document.
  """
  @spec from_document(A2ML.Types.Document.t()) :: t()
  def from_document(%A2ML.Types.Document{} = doc) do
    version =
      doc.directives
      |> Enum.find(fn d -> d.name == "version" end)
      |> case do
        nil -> nil
        d -> d.value
      end

    %__MODULE__{
      version: version,
      title: doc.title,
      directives: doc.directives,
      attestations: doc.attestations
    }
  end
end
