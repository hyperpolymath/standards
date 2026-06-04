# SPDX-License-Identifier: MPL-2.0
# (PMPL-1.0-or-later preferred; MPL-2.0 required for Hex.pm)

defmodule K9.Types do
  @moduledoc """
  Core data types for K9 (Self-Validating Components).

  Defines the structures for K9 component specifications, including
  pedigree metadata, security levels, target platforms, recipes, and contracts.
  """
end

defmodule K9.Types.Component do
  @moduledoc """
  A K9 self-validating component. This is the top-level AST node.
  """

  @type t :: %__MODULE__{
          pedigree: K9.Types.Pedigree.t(),
          security: K9.Types.SecurityPolicy.t(),
          target: K9.Types.Target.t() | nil,
          recipes: K9.Types.Recipes.t() | nil,
          validation: K9.Types.Validation.t() | nil,
          content: %{String.t() => String.t()},
          tags: [String.t()]
        }

  defstruct pedigree: nil,
            security: nil,
            target: nil,
            recipes: nil,
            validation: nil,
            content: %{},
            tags: []
end

defmodule K9.Types.Pedigree do
  @moduledoc """
  Pedigree: identity and provenance metadata for a K9 component.
  """

  @type t :: %__MODULE__{
          name: String.t(),
          version: String.t(),
          description: String.t(),
          author: String.t() | nil,
          license: String.t() | nil
        }

  defstruct name: "",
            version: "0.1.0",
            description: "",
            author: nil,
            license: nil
end

defmodule K9.Types.SecurityLevel do
  @moduledoc """
  K9 security levels forming a trust hierarchy.

  - `:kennel` — Pure data, no execution, safe anywhere.
  - `:yard`   — Controlled execution, limited permissions.
  - `:hunt`   — Full execution with explicit authorisation required.
  """

  @type t :: :kennel | :yard | :hunt

  @doc """
  Parse a security level from its canonical string representation.
  """
  @spec from_string(String.t()) :: {:ok, t()} | {:error, :unknown_security_level}
  def from_string(s) do
    case String.downcase(String.trim(s)) do
      "kennel" -> {:ok, :kennel}
      "yard" -> {:ok, :yard}
      "hunt" -> {:ok, :hunt}
      _ -> {:error, :unknown_security_level}
    end
  end

  @doc """
  Return the canonical string representation of a security level.
  """
  @spec to_string(t()) :: String.t()
  def to_string(:kennel), do: "kennel"
  def to_string(:yard), do: "yard"
  def to_string(:hunt), do: "hunt"

  @doc """
  Compare two security levels. Returns `:lt`, `:eq`, or `:gt`.
  """
  @spec compare(t(), t()) :: :lt | :eq | :gt
  def compare(a, b), do: compare_int(level_to_int(a), level_to_int(b))

  defp level_to_int(:kennel), do: 0
  defp level_to_int(:yard), do: 1
  defp level_to_int(:hunt), do: 2

  defp compare_int(a, b) when a < b, do: :lt
  defp compare_int(a, b) when a > b, do: :gt
  defp compare_int(_, _), do: :eq
end

defmodule K9.Types.SecurityPolicy do
  @moduledoc """
  Security policy combining the level with specific permission flags.
  """

  @type t :: %__MODULE__{
          level: K9.Types.SecurityLevel.t(),
          allow_network: boolean(),
          allow_fs_write: boolean(),
          allow_subprocess: boolean()
        }

  defstruct level: :kennel,
            allow_network: false,
            allow_fs_write: false,
            allow_subprocess: false
end

defmodule K9.Types.Target do
  @moduledoc """
  Target platform constraints.
  """

  @type t :: %__MODULE__{
          os: String.t() | nil,
          is_edge: boolean(),
          requires_podman: boolean(),
          memory: String.t() | nil
        }

  defstruct os: nil,
            is_edge: false,
            requires_podman: false,
            memory: nil
end

defmodule K9.Types.Recipe do
  @moduledoc """
  Named recipe for lifecycle operations.
  """

  @type t :: %__MODULE__{
          name: String.t(),
          command: String.t()
        }

  defstruct name: "",
            command: ""
end

defmodule K9.Types.Recipes do
  @moduledoc """
  Collection of standard lifecycle recipes.
  """

  @type t :: %__MODULE__{
          install: String.t() | nil,
          validate: String.t() | nil,
          deploy: String.t() | nil,
          migrate: String.t() | nil,
          custom: %{String.t() => String.t()}
        }

  defstruct install: nil,
            validate: nil,
            deploy: nil,
            migrate: nil,
            custom: %{}
end

defmodule K9.Types.Validation do
  @moduledoc """
  Self-validation block.
  """

  @type t :: %__MODULE__{
          checksum: String.t(),
          pedigree_version: String.t(),
          hunt_authorized: boolean()
        }

  defstruct checksum: "",
            pedigree_version: "1.0",
            hunt_authorized: false
end

defmodule K9.Types.Contract do
  @moduledoc """
  A contract attached to a K9 component (from the contractile system).
  """

  @type t :: %__MODULE__{
          name: String.t(),
          clauses: [K9.Types.ContractClause.t()]
        }

  defstruct name: "",
            clauses: []
end

defmodule K9.Types.ContractClause do
  @moduledoc """
  A single clause within a K9 contract.
  """

  @type t :: %__MODULE__{
          clause_type: String.t(),
          predicate: String.t(),
          verified: boolean()
        }

  defstruct clause_type: "",
            predicate: "",
            verified: false
end
