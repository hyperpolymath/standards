defmodule Palimpsest.Metadata.Lineage do
  @moduledoc """
  Ecto schema for tracking lineage chains of creative works.

  Implements quantum-proof traceability for complete chain-of-custody
  from original work through all derivatives.
  """

  use Ecto.Schema
  import Ecto.Changeset

  @primary_key {:id, :binary_id, autogenerate: true}
  @foreign_key_type :binary_id

  schema "palimpsest_lineages" do
    field :lineage_id, :string

    # Root work (original)
    embeds_one :root_work, RootWork, on_replace: :update do
      field :uri, :string
      field :work_id, :string
      field :creator, :string
      field :title, :string
      field :created_at, :utc_datetime
    end

    # Chain of transformations
    embeds_many :chain, ChainLink, on_replace: :delete do
      field :uri, :string
      field :work_id, :string
      field :creator, :string
      field :title, :string
      field :timestamp, :utc_datetime
      field :transformation_type, Ecto.Enum,
        values: [:original, :remix, :translation, :adaptation, :commentary, :extension, :derivative]

      field :generation_number, :integer
      field :parent_uri, :string
      field :hash, :string
    end

    # Quantum-proof cryptographic verification
    embeds_one :quantum_proof, QuantumProof, on_replace: :update do
      field :algorithm, Ecto.Enum,
        values: [:"SPHINCS+", :"CRYSTALS-Dilithium", :Falcon, :other]

      field :signature, :string
      field :public_key, :string
      field :signed_at, :utc_datetime
      field :key_id, :string

      # Merkle tree for efficient verification
      field :merkle_root, :string
      field :merkle_proof, {:array, :string}
    end

    # Blockchain/DLT integration
    embeds_one :blockchain, BlockchainRecord, on_replace: :update do
      field :network, :string
      field :transaction_hash, :string
      field :block_number, :integer
      field :contract_address, :string
      field :timestamp, :utc_datetime
    end

    # IPFS/decentralised storage
    embeds_one :decentralised_storage, DecentralisedStorage, on_replace: :update do
      field :ipfs_hash, :string
      field :arweave_id, :string
      field :filecoin_cid, :string
      field :other_identifiers, :map
    end

    # Verification status
    field :verified, :boolean, default: false
    field :verification_date, :utc_datetime
    field :verifier, :string

    # Metadata
    field :total_generations, :integer
    field :branch_count, :integer
    field :last_update, :utc_datetime

    timestamps(type: :utc_datetime)
  end

  @doc """
  Changeset for creating or updating a lineage record.
  """
  def changeset(lineage, attrs) do
    lineage
    |> cast(attrs, [
      :lineage_id,
      :verified,
      :verification_date,
      :verifier,
      :total_generations,
      :branch_count,
      :last_update
    ])
    |> cast_embed(:root_work, with: &root_work_changeset/2, required: true)
    |> cast_embed(:chain, with: &chain_link_changeset/2, required: true)
    |> cast_embed(:quantum_proof, with: &quantum_proof_changeset/2)
    |> cast_embed(:blockchain, with: &blockchain_changeset/2)
    |> cast_embed(:decentralised_storage, with: &decentralised_storage_changeset/2)
    |> validate_required([:lineage_id])
    |> validate_format(:lineage_id, ~r/^lineage-[a-z0-9-]+$/)
    |> unique_constraint(:lineage_id)
    |> validate_chain_integrity()
  end

  defp root_work_changeset(work, attrs) do
    work
    |> cast(attrs, [:uri, :work_id, :creator, :title, :created_at])
    |> validate_required([:uri, :creator])
    |> validate_format(:uri, ~r/^https?:\/\//)
  end

  defp chain_link_changeset(link, attrs) do
    link
    |> cast(attrs, [
      :uri,
      :work_id,
      :creator,
      :title,
      :timestamp,
      :transformation_type,
      :generation_number,
      :parent_uri,
      :hash
    ])
    |> validate_required([
      :uri,
      :creator,
      :timestamp,
      :transformation_type,
      :generation_number
    ])
    |> validate_number(:generation_number, greater_than_or_equal_to: 0)
  end

  defp quantum_proof_changeset(proof, attrs) do
    proof
    |> cast(attrs, [
      :algorithm,
      :signature,
      :public_key,
      :signed_at,
      :key_id,
      :merkle_root,
      :merkle_proof
    ])
    |> validate_required([:algorithm, :signature, :public_key])
    |> validate_quantum_signature()
  end

  defp blockchain_changeset(blockchain, attrs) do
    blockchain
    |> cast(attrs, [
      :network,
      :transaction_hash,
      :block_number,
      :contract_address,
      :timestamp
    ])
    |> validate_required([:network, :transaction_hash])
  end

  defp decentralised_storage_changeset(storage, attrs) do
    storage
    |> cast(attrs, [:ipfs_hash, :arweave_id, :filecoin_cid, :other_identifiers])
    |> validate_at_least_one_storage()
  end

  # Custom validators

  defp validate_chain_integrity(changeset) do
    chain = get_field(changeset, :chain) || []
    root_work = get_field(changeset, :root_work)

    cond do
      Enum.empty?(chain) ->
        add_error(changeset, :chain, "must have at least one link (the original work)")

      is_nil(root_work) ->
        add_error(changeset, :root_work, "must be specified")

      true ->
        changeset
        |> validate_chain_sequence()
        |> validate_generation_numbers()
    end
  end

  defp validate_chain_sequence(changeset) do
    chain = get_field(changeset, :chain) || []

    # First link should be the original
    case List.first(chain) do
      %{transformation_type: :original} ->
        changeset

      _ ->
        add_error(changeset, :chain, "first link must be the original work")
    end
  end

  defp validate_generation_numbers(changeset) do
    chain = get_field(changeset, :chain) || []

    generations = Enum.map(chain, & &1.generation_number)
    expected = Enum.to_list(0..(length(chain) - 1))

    if Enum.sort(generations) == expected do
      changeset
    else
      add_error(changeset, :chain, "generation numbers must be sequential starting from 0")
    end
  end

  defp validate_quantum_signature(changeset) do
    algorithm = get_field(changeset, :algorithm)
    signature = get_field(changeset, :signature)

    # Basic validation - in production, would verify signature
    if algorithm && signature && String.length(signature) < 10 do
      add_error(changeset, :signature, "appears to be invalid or too short")
    else
      changeset
    end
  end

  defp validate_at_least_one_storage(changeset) do
    ipfs = get_field(changeset, :ipfs_hash)
    arweave = get_field(changeset, :arweave_id)
    filecoin = get_field(changeset, :filecoin_cid)

    if is_nil(ipfs) && is_nil(arweave) && is_nil(filecoin) do
      add_error(
        changeset,
        :decentralised_storage,
        "must provide at least one decentralised storage identifier"
      )
    else
      changeset
    end
  end

  @doc """
  Returns the length of the lineage chain.
  """
  def chain_length(%__MODULE__{chain: chain}), do: length(chain || [])

  @doc """
  Returns the current generation number (latest in chain).
  """
  def current_generation(%__MODULE__{chain: chain}) do
    chain
    |> List.last()
    |> case do
      nil -> 0
      link -> link.generation_number
    end
  end

  @doc """
  Returns all creators in the lineage chain.
  """
  def all_creators(%__MODULE__{chain: chain}) do
    chain
    |> Enum.map(& &1.creator)
    |> Enum.uniq()
  end

  @doc """
  Verifies the quantum-proof signature (simplified - actual implementation would use crypto library).
  """
  def verify_signature(%__MODULE__{quantum_proof: nil}), do: {:error, :no_signature}

  def verify_signature(%__MODULE__{quantum_proof: proof}) do
    # In production, this would use a proper post-quantum cryptography library
    # For now, just check that required fields are present
    if proof.signature && proof.public_key && proof.algorithm do
      {:ok, :signature_present}
    else
      {:error, :incomplete_signature}
    end
  end

  @doc """
  Adds a new link to the lineage chain.
  """
  def add_link(lineage, link_attrs) do
    current_gen = current_generation(lineage)
    link_attrs = Map.put(link_attrs, :generation_number, current_gen + 1)

    updated_chain = (lineage.chain || []) ++ [link_attrs]

    lineage
    |> change()
    |> put_embed(:chain, updated_chain)
    |> put_change(:total_generations, current_gen + 2)
    |> put_change(:last_update, DateTime.utc_now())
  end
end
