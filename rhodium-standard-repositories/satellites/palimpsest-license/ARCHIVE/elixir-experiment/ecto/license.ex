defmodule Palimpsest.Metadata.License do
  @moduledoc """
  Ecto schema for Palimpsest License metadata.

  This schema represents the core license information including version,
  governance structure, and legal jurisdiction details.
  """

  use Ecto.Schema
  import Ecto.Changeset

  @primary_key {:id, :binary_id, autogenerate: true}
  @foreign_key_type :binary_id

  schema "palimpsest_licenses" do
    field :license_id, :string
    field :version_major, :integer
    field :version_minor, :integer
    field :version_patch, :integer
    field :version_status, Ecto.Enum, values: [:draft, :in_development, :stable, :deprecated, :archived]

    # Multilingual titles
    field :title_en, :string
    field :title_nl, :string
    embeds_one :title_translations, TitleTranslations, on_replace: :update do
      field :de, :string
      field :fr, :string
      field :es, :string
      field :other, :map
    end

    # Multilingual descriptions
    field :description_en, :string
    field :description_nl, :string

    # Governance
    embeds_one :governance, Governance, on_replace: :update do
      field :council_name, :string
      field :member_count, :integer
      field :draft_voting_threshold, :string
      field :ratification_voting_threshold, :string
      field :major_version_voting_threshold, :string
      field :consultation_period_days, :integer
      field :consultation_period_description, :string
    end

    # Protection features
    embeds_one :protection, Protection, on_replace: :update do
      field :emotional_lineage, :boolean, default: true
      field :quantum_proof_traceability, :boolean, default: true
      field :symbolic_attribution, :boolean, default: true
      field :ai_consent_required, :boolean, default: true
      field :metadata_preservation_mandatory, :boolean, default: true
    end

    # Jurisdiction
    embeds_one :jurisdiction, Jurisdiction, on_replace: :update do
      field :primary_country, :string
      field :enforcement_venue, :string
      field :governing_law, :string
      field :dispute_resolution_methods, {:array, :string}
      field :international_framework, :string
    end

    # Metadata
    field :created_at, :utc_datetime
    field :modified_at, :utc_datetime
    field :published_at, :utc_datetime

    # URIs and identifiers
    field :canonical_uri, :string
    field :doi, :string
    field :spdx_identifier, :string

    # Relations
    field :replaces_license_id, :string
    field :superseded_by_license_id, :string

    timestamps(type: :utc_datetime)
  end

  @doc """
  Changeset for creating or updating a license record.
  """
  def changeset(license, attrs) do
    license
    |> cast(attrs, [
      :license_id,
      :version_major,
      :version_minor,
      :version_patch,
      :version_status,
      :title_en,
      :title_nl,
      :description_en,
      :description_nl,
      :created_at,
      :modified_at,
      :published_at,
      :canonical_uri,
      :doi,
      :spdx_identifier,
      :replaces_license_id,
      :superseded_by_license_id
    ])
    |> cast_embed(:title_translations)
    |> cast_embed(:governance, with: &governance_changeset/2)
    |> cast_embed(:protection, with: &protection_changeset/2)
    |> cast_embed(:jurisdiction, with: &jurisdiction_changeset/2)
    |> validate_required([
      :license_id,
      :version_major,
      :version_minor,
      :title_en,
      :canonical_uri
    ])
    |> validate_number(:version_major, greater_than_or_equal_to: 0)
    |> validate_number(:version_minor, greater_than_or_equal_to: 0)
    |> validate_number(:version_patch, greater_than_or_equal_to: 0)
    |> validate_format(:license_id, ~r/^palimpsest-v\d+\.\d+/)
    |> unique_constraint(:license_id)
    |> unique_constraint(:canonical_uri)
  end

  defp governance_changeset(governance, attrs) do
    governance
    |> cast(attrs, [
      :council_name,
      :member_count,
      :draft_voting_threshold,
      :ratification_voting_threshold,
      :major_version_voting_threshold,
      :consultation_period_days,
      :consultation_period_description
    ])
    |> validate_required([:council_name, :member_count])
    |> validate_number(:member_count, greater_than: 0)
    |> validate_format(:draft_voting_threshold, ~r/^\d+\/\d+$/)
    |> validate_format(:ratification_voting_threshold, ~r/^\d+\/\d+$/)
    |> validate_format(:major_version_voting_threshold, ~r/^\d+\/\d+$/)
  end

  defp protection_changeset(protection, attrs) do
    protection
    |> cast(attrs, [
      :emotional_lineage,
      :quantum_proof_traceability,
      :symbolic_attribution,
      :ai_consent_required,
      :metadata_preservation_mandatory
    ])
    |> validate_required([
      :emotional_lineage,
      :quantum_proof_traceability,
      :symbolic_attribution
    ])
  end

  defp jurisdiction_changeset(jurisdiction, attrs) do
    jurisdiction
    |> cast(attrs, [
      :primary_country,
      :enforcement_venue,
      :governing_law,
      :dispute_resolution_methods,
      :international_framework
    ])
    |> validate_required([:primary_country, :governing_law])
  end

  @doc """
  Returns the full semantic version string.
  """
  def version_string(%__MODULE__{} = license) do
    "#{license.version_major}.#{license.version_minor}.#{license.version_patch || 0}"
  end

  @doc """
  Returns the license identifier with version.
  """
  def full_identifier(%__MODULE__{} = license) do
    "palimpsest-v#{version_string(license)}"
  end
end
