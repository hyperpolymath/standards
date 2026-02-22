defmodule Palimpsest.Metadata.Work do
  @moduledoc """
  Ecto schema for creative works licensed under the Palimpsest License.

  This schema captures the emotional lineage, thematic integrity, and AI consent
  settings for original creative works.
  """

  use Ecto.Schema
  import Ecto.Changeset

  @primary_key {:id, :binary_id, autogenerate: true}
  @foreign_key_type :binary_id

  schema "palimpsest_works" do
    field :work_id, :string
    field :canonical_uri, :string

    # License version this work uses
    field :license_id, :string
    field :license_version, :string

    # Multilingual titles and descriptions
    field :title_en, :string
    field :title_nl, :string
    embeds_one :title_translations, TitleTranslations, on_replace: :update do
      field :other, :map
    end

    field :description_en, :string
    field :description_nl, :string

    # Creator information
    embeds_one :creator, Creator, on_replace: :update do
      field :name, :string
      field :uri, :string
      field :email, :string
      field :orcid, :string
      field :wallet_address, :string
      field :public_key, :string
    end

    # Emotional lineage
    embeds_one :emotional_lineage, EmotionalLineage, on_replace: :update do
      field :context, :string
      field :themes, {:array, :string}
      field :sensitivities, {:array, :string}

      embeds_many :symbolic_elements, SymbolicElement, on_replace: :delete do
        field :symbol, :string
        field :meaning, :string
        field :cultural_origin, :string
      end
    end

    # Thematic integrity
    embeds_one :thematic_integrity, ThematicIntegrity, on_replace: :update do
      field :core_themes, {:array, :string}
      field :prohibited_uses, {:array, :string}
      field :interpretation_notes, :string
    end

    # AI consent
    embeds_one :ai_consent, AIConsent, on_replace: :update do
      field :training_allowed, :boolean, default: false
      field :generation_allowed, :boolean, default: false
      field :conditions, {:array, :string}
      field :exceptions, {:array, :string}
      field :expiry_date, :date
    end

    # Work metadata
    embeds_one :work_metadata, WorkMetadata, on_replace: :update do
      field :work_type, :string
      field :medium, :string
      field :genre, {:array, :string}
      field :language, {:array, :string}
      field :tags, {:array, :string}
      field :doi, :string
      field :isbn, :string
      field :issn, :string
      field :external_identifiers, :map
    end

    # Accessibility
    embeds_one :accessibility, Accessibility, on_replace: :update do
      field :formats_available, {:array, :string}
      field :screen_reader_compatible, :boolean
      field :plain_language_summary, :string
      field :low_bandwidth_version_uri, :string
      field :accessibility_notes, :string
    end

    # Quantum-proof tracking
    field :quantum_proof_hash, :string
    field :quantum_algorithm, :string
    field :quantum_signature, :string
    field :quantum_public_key, :string

    # Timestamps
    field :created_at, :utc_datetime
    field :modified_at, :utc_datetime
    field :published_at, :utc_datetime
    field :registered_at, :utc_datetime

    # Status
    field :status, Ecto.Enum, values: [:draft, :published, :archived, :disputed]

    # Relations (storing IDs as strings for flexibility)
    field :parent_work_id, :string
    field :collection_id, :string

    timestamps(type: :utc_datetime)
  end

  @doc """
  Changeset for creating or updating a work record.
  """
  def changeset(work, attrs) do
    work
    |> cast(attrs, [
      :work_id,
      :canonical_uri,
      :license_id,
      :license_version,
      :title_en,
      :title_nl,
      :description_en,
      :description_nl,
      :quantum_proof_hash,
      :quantum_algorithm,
      :quantum_signature,
      :quantum_public_key,
      :created_at,
      :modified_at,
      :published_at,
      :registered_at,
      :status,
      :parent_work_id,
      :collection_id
    ])
    |> cast_embed(:title_translations)
    |> cast_embed(:creator, with: &creator_changeset/2)
    |> cast_embed(:emotional_lineage, with: &emotional_lineage_changeset/2)
    |> cast_embed(:thematic_integrity, with: &thematic_integrity_changeset/2)
    |> cast_embed(:ai_consent, with: &ai_consent_changeset/2)
    |> cast_embed(:work_metadata, with: &work_metadata_changeset/2)
    |> cast_embed(:accessibility, with: &accessibility_changeset/2)
    |> validate_required([
      :work_id,
      :canonical_uri,
      :license_id,
      :title_en
    ])
    |> validate_format(:canonical_uri, ~r/^https?:\/\//)
    |> validate_format(:work_id, ~r/^work-[a-z0-9-]+$/)
    |> unique_constraint(:work_id)
    |> unique_constraint(:canonical_uri)
  end

  defp creator_changeset(creator, attrs) do
    creator
    |> cast(attrs, [:name, :uri, :email, :orcid, :wallet_address, :public_key])
    |> validate_required([:name])
    |> validate_format(:email, ~r/@/, message: "must be a valid email")
    |> validate_format(:orcid, ~r/^\d{4}-\d{4}-\d{4}-\d{3}[0-9X]$/,
      message: "must be a valid ORCID format"
    )
  end

  defp emotional_lineage_changeset(lineage, attrs) do
    lineage
    |> cast(attrs, [:context, :themes, :sensitivities])
    |> cast_embed(:symbolic_elements, with: &symbolic_element_changeset/2)
    |> validate_required([:context])
    |> validate_length(:themes, min: 1, message: "must have at least one theme")
  end

  defp symbolic_element_changeset(element, attrs) do
    element
    |> cast(attrs, [:symbol, :meaning, :cultural_origin])
    |> validate_required([:symbol, :meaning])
  end

  defp thematic_integrity_changeset(integrity, attrs) do
    integrity
    |> cast(attrs, [:core_themes, :prohibited_uses, :interpretation_notes])
    |> validate_required([:core_themes])
    |> validate_subset(:prohibited_uses, [
      "trivialisation",
      "commodification",
      "misrepresentation",
      "flattening"
    ])
  end

  defp ai_consent_changeset(consent, attrs) do
    consent
    |> cast(attrs, [
      :training_allowed,
      :generation_allowed,
      :conditions,
      :exceptions,
      :expiry_date
    ])
    |> validate_required([:training_allowed, :generation_allowed])
    |> validate_ai_consent_consistency()
  end

  defp work_metadata_changeset(metadata, attrs) do
    metadata
    |> cast(attrs, [
      :work_type,
      :medium,
      :genre,
      :language,
      :tags,
      :doi,
      :isbn,
      :issn,
      :external_identifiers
    ])
  end

  defp accessibility_changeset(accessibility, attrs) do
    accessibility
    |> cast(attrs, [
      :formats_available,
      :screen_reader_compatible,
      :plain_language_summary,
      :low_bandwidth_version_uri,
      :accessibility_notes
    ])
    |> validate_accessibility_requirements()
  end

  # Custom validators

  defp validate_subset(changeset, field, allowed_values) do
    validate_change(changeset, field, fn _, values ->
      invalid = Enum.reject(values, &(&1 in allowed_values))

      case invalid do
        [] -> []
        _ -> [{field, "contains invalid values: #{Enum.join(invalid, ", ")}"}]
      end
    end)
  end

  defp validate_ai_consent_consistency(changeset) do
    training = get_field(changeset, :training_allowed)
    generation = get_field(changeset, :generation_allowed)
    conditions = get_field(changeset, :conditions) || []

    cond do
      training == false && generation == false && conditions == [] ->
        add_error(
          changeset,
          :conditions,
          "should specify reasons when both training and generation are disallowed"
        )

      true ->
        changeset
    end
  end

  defp validate_accessibility_requirements(changeset) do
    formats = get_field(changeset, :formats_available) || []
    screen_reader = get_field(changeset, :screen_reader_compatible)

    if Enum.empty?(formats) && is_nil(screen_reader) do
      add_error(
        changeset,
        :accessibility,
        "must specify at least one accessibility feature"
      )
    else
      changeset
    end
  end

  @doc """
  Returns whether AI training is permitted for this work.
  """
  def ai_training_allowed?(%__MODULE__{ai_consent: nil}), do: false
  def ai_training_allowed?(%__MODULE__{ai_consent: consent}), do: consent.training_allowed

  @doc """
  Returns whether AI generation is permitted for this work.
  """
  def ai_generation_allowed?(%__MODULE__{ai_consent: nil}), do: false
  def ai_generation_allowed?(%__MODULE__{ai_consent: consent}), do: consent.generation_allowed

  @doc """
  Returns the sensitivity level of this work based on emotional lineage.
  """
  def sensitivity_level(%__MODULE__{emotional_lineage: nil}), do: :standard

  def sensitivity_level(%__MODULE__{emotional_lineage: lineage}) do
    sensitive_categories = ["trauma", "sacred", "cultural-heritage", "indigenous"]

    case Enum.any?(lineage.sensitivities || [], &(&1 in sensitive_categories)) do
      true -> :high
      false -> :standard
    end
  end
end
