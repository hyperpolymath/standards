defmodule Palimpsest.Metadata.Derivative do
  @moduledoc """
  Ecto schema for derivative works under the Palimpsest License.

  This schema tracks remixes, adaptations, and AI-generated derivatives,
  ensuring proper lineage statements and reciprocity.
  """

  use Ecto.Schema
  import Ecto.Changeset

  @primary_key {:id, :binary_id, autogenerate: true}
  @foreign_key_type :binary_id

  schema "palimpsest_derivatives" do
    field :derivative_id, :string
    field :canonical_uri, :string

    # License information
    field :license_id, :string
    field :license_version, :string

    # Titles
    field :title_en, :string
    field :title_nl, :string

    # Original work reference
    embeds_one :original_work, OriginalWork, on_replace: :update do
      field :uri, :string
      field :work_id, :string
      field :creator, :string
      field :title, :string
    end

    # Derivative creator
    embeds_one :derivative_creator, DerivativeCreator, on_replace: :update do
      field :name, :string
      field :uri, :string
      field :email, :string
      field :orcid, :string
    end

    # Lineage statement (required by Clause 3.2)
    embeds_one :lineage_statement, LineageStatement, on_replace: :update do
      embeds_one :attribution, Attribution, on_replace: :update do
        field :type, Ecto.Enum, values: [:direct, :indirect, :environmental, :symbolic]
        field :content, :string
        field :preserves_context, :boolean
        field :display_format, :string
      end

      embeds_many :transformations, Transformation, on_replace: :delete do
        field :type, Ecto.Enum,
          values: [:remix, :translation, :adaptation, :commentary, :extension]

        field :description, :string
        field :preserves_integrity, :boolean
        field :technical_details, :string
      end

      embeds_one :reciprocity, Reciprocity, on_replace: :update do
        field :offered, :boolean
        field :type, Ecto.Enum,
          values: [:percentage_share, :archive_credit, :attribution, :other]

        field :details, :string
        field :contract_uri, :string
        field :amount_percentage, :decimal
      end
    end

    # Commercial use flag
    field :commercial_use, :boolean, default: false

    # Synthetic/AI lineage tag (required for AI-generated works)
    embeds_one :synthetic_lineage_tag, SyntheticLineageTag, on_replace: :update do
      field :is_synthetic, :boolean, default: false
      field :ai_system, :string
      field :model_name, :string
      field :model_version, :string
      field :training_consent_verified, :boolean
      field :generation_date, :utc_datetime
      field :quantum_proof_hash, :string
      field :prompt_used, :string
      field :human_oversight, :boolean
      field :post_generation_editing, :boolean
    end

    # Compliance verification
    embeds_one :compliance, Compliance, on_replace: :update do
      field :thematic_integrity_preserved, :boolean
      field :symbolic_attribution_provided, :boolean
      field :metadata_preserved, :boolean
      field :accessibility_maintained, :boolean
      field :review_date, :date
      field :reviewer, :string
      field :notes, :string
    end

    # Timestamps
    field :created_at, :utc_datetime
    field :published_at, :utc_datetime
    field :registered_at, :utc_datetime

    # Status
    field :status, Ecto.Enum, values: [:draft, :published, :disputed, :revoked]

    timestamps(type: :utc_datetime)
  end

  @doc """
  Changeset for creating or updating a derivative work.
  """
  def changeset(derivative, attrs) do
    derivative
    |> cast(attrs, [
      :derivative_id,
      :canonical_uri,
      :license_id,
      :license_version,
      :title_en,
      :title_nl,
      :commercial_use,
      :created_at,
      :published_at,
      :registered_at,
      :status
    ])
    |> cast_embed(:original_work, with: &original_work_changeset/2)
    |> cast_embed(:derivative_creator, with: &derivative_creator_changeset/2)
    |> cast_embed(:lineage_statement, with: &lineage_statement_changeset/2, required: true)
    |> cast_embed(:synthetic_lineage_tag, with: &synthetic_lineage_tag_changeset/2)
    |> cast_embed(:compliance, with: &compliance_changeset/2)
    |> validate_required([
      :derivative_id,
      :canonical_uri,
      :license_id,
      :title_en
    ])
    |> validate_format(:derivative_id, ~r/^derivative-[a-z0-9-]+$/)
    |> unique_constraint(:derivative_id)
    |> unique_constraint(:canonical_uri)
    |> validate_commercial_reciprocity()
    |> validate_synthetic_requirements()
  end

  defp original_work_changeset(work, attrs) do
    work
    |> cast(attrs, [:uri, :work_id, :creator, :title])
    |> validate_required([:uri])
    |> validate_format(:uri, ~r/^https?:\/\//)
  end

  defp derivative_creator_changeset(creator, attrs) do
    creator
    |> cast(attrs, [:name, :uri, :email, :orcid])
    |> validate_required([:name])
    |> validate_format(:email, ~r/@/)
    |> validate_format(:orcid, ~r/^\d{4}-\d{4}-\d{4}-\d{3}[0-9X]$/)
  end

  defp lineage_statement_changeset(statement, attrs) do
    statement
    |> cast(attrs, [])
    |> cast_embed(:attribution, with: &attribution_changeset/2, required: true)
    |> cast_embed(:transformations, with: &transformation_changeset/2, required: true)
    |> cast_embed(:reciprocity, with: &reciprocity_changeset/2)
    |> validate_required([:attribution, :transformations])
  end

  defp attribution_changeset(attribution, attrs) do
    attribution
    |> cast(attrs, [:type, :content, :preserves_context, :display_format])
    |> validate_required([:type, :content, :preserves_context])
    |> validate_context_preservation()
  end

  defp transformation_changeset(transformation, attrs) do
    transformation
    |> cast(attrs, [:type, :description, :preserves_integrity, :technical_details])
    |> validate_required([:type, :description, :preserves_integrity])
  end

  defp reciprocity_changeset(reciprocity, attrs) do
    reciprocity
    |> cast(attrs, [:offered, :type, :details, :contract_uri, :amount_percentage])
    |> validate_required([:offered])
    |> validate_reciprocity_details()
  end

  defp synthetic_lineage_tag_changeset(tag, attrs) do
    tag
    |> cast(attrs, [
      :is_synthetic,
      :ai_system,
      :model_name,
      :model_version,
      :training_consent_verified,
      :generation_date,
      :quantum_proof_hash,
      :prompt_used,
      :human_oversight,
      :post_generation_editing
    ])
    |> validate_synthetic_tag()
  end

  defp compliance_changeset(compliance, attrs) do
    compliance
    |> cast(attrs, [
      :thematic_integrity_preserved,
      :symbolic_attribution_provided,
      :metadata_preserved,
      :accessibility_maintained,
      :review_date,
      :reviewer,
      :notes
    ])
    |> validate_required([
      :thematic_integrity_preserved,
      :symbolic_attribution_provided,
      :metadata_preserved
    ])
  end

  # Custom validators

  defp validate_context_preservation(changeset) do
    preserves = get_field(changeset, :preserves_context)

    if preserves == false do
      add_error(
        changeset,
        :preserves_context,
        "Attribution must preserve cultural and emotional context (Clause 1.1)"
      )
    else
      changeset
    end
  end

  defp validate_reciprocity_details(changeset) do
    offered = get_field(changeset, :offered)
    type = get_field(changeset, :type)
    details = get_field(changeset, :details)

    if offered && is_nil(type) do
      add_error(changeset, :type, "must specify reciprocity type when offered")
    else
      if offered && is_nil(details) do
        add_error(changeset, :details, "must provide reciprocity details when offered")
      else
        changeset
      end
    end
  end

  defp validate_commercial_reciprocity(changeset) do
    commercial = get_field(changeset, :commercial_use)
    lineage = get_field(changeset, :lineage_statement)

    if commercial && lineage do
      reciprocity_offered = get_in(lineage, [Access.key(:reciprocity), Access.key(:offered)])

      if reciprocity_offered == false do
        add_error(
          changeset,
          :commercial_use,
          "Commercial use requires reciprocity (Clause 3.1)"
        )
      else
        changeset
      end
    else
      changeset
    end
  end

  defp validate_synthetic_tag(changeset) do
    is_synthetic = get_field(changeset, :is_synthetic)

    if is_synthetic do
      changeset
      |> validate_required([
        :ai_system,
        :model_name,
        :training_consent_verified,
        :generation_date,
        :quantum_proof_hash
      ])
      |> validate_training_consent()
    else
      changeset
    end
  end

  defp validate_synthetic_requirements(changeset) do
    synthetic_tag = get_field(changeset, :synthetic_lineage_tag)

    if synthetic_tag && synthetic_tag.is_synthetic do
      if is_nil(synthetic_tag.quantum_proof_hash) do
        add_error(
          changeset,
          :synthetic_lineage_tag,
          "Quantum-proof hash required for AI-generated derivatives (Clause 6.3)"
        )
      else
        changeset
      end
    else
      changeset
    end
  end

  defp validate_training_consent(changeset) do
    consent_verified = get_field(changeset, :training_consent_verified)

    if consent_verified == false do
      add_error(
        changeset,
        :training_consent_verified,
        "Training consent must be verified for AI-generated derivatives (Clause 6.2)"
      )
    else
      changeset
    end
  end

  @doc """
  Returns whether this derivative is AI-generated.
  """
  def ai_generated?(%__MODULE__{synthetic_lineage_tag: nil}), do: false

  def ai_generated?(%__MODULE__{synthetic_lineage_tag: tag}),
    do: tag.is_synthetic

  @doc """
  Returns whether this derivative complies with Palimpsest requirements.
  """
  def compliant?(%__MODULE__{compliance: nil}), do: false

  def compliant?(%__MODULE__{compliance: compliance}) do
    compliance.thematic_integrity_preserved &&
      compliance.symbolic_attribution_provided &&
      compliance.metadata_preserved
  end

  @doc """
  Returns the reciprocity type if offered.
  """
  def reciprocity_type(%__MODULE__{lineage_statement: nil}), do: nil

  def reciprocity_type(%__MODULE__{lineage_statement: statement}) do
    if statement.reciprocity && statement.reciprocity.offered do
      statement.reciprocity.type
    else
      nil
    end
  end
end
