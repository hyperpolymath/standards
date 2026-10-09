defmodule Palimpsest.Metadata.Violation do
  @moduledoc """
  Ecto schema for tracking license violations and their resolutions.

  Supports the dispute resolution process outlined in Clause 8 (Symbolic Defence).
  """

  use Ecto.Schema
  import Ecto.Changeset

  @primary_key {:id, :binary_id, autogenerate: true}
  @foreign_key_type :binary_id

  schema "palimpsest_violations" do
    field :violation_id, :string

    # Work that was violated
    field :work_uri, :string
    field :work_id, :string
    field :work_title, :string

    # Violation details
    field :violation_type, Ecto.Enum,
      values: [
        :attribution_failure,
        :metadata_stripping,
        :cultural_misrepresentation,
        :unauthorised_ai_training,
        :unauthorised_ai_generation,
        :trivialisation,
        :commodification,
        :flattening,
        :accessibility_breach,
        :reciprocity_failure,
        :other
      ]

    field :description, :string
    field :detailed_explanation, :string

    # Reporter information
    embeds_one :reported_by, Reporter, on_replace: :update do
      field :name, :string
      field :email, :string
      field :uri, :string
      field :relationship, Ecto.Enum,
        values: [:creator, :rights_holder, :community_member, :third_party, :automated_system]

      field :anonymous, :boolean, default: false
    end

    # Alleged violator
    embeds_one :alleged_violator, AllegedViolator, on_replace: :update do
      field :name, :string
      field :organisation, :string
      field :uri, :string
      field :contact_attempted, :boolean
      field :contact_date, :date
      field :response_received, :boolean
    end

    # Evidence
    embeds_many :evidence, Evidence, on_replace: :delete do
      field :type, Ecto.Enum,
        values: [:url, :screenshot, :document, :testimony, :technical_analysis, :blockchain_record]

      field :content, :string
      field :uri, :string
      field :ipfs_hash, :string
      field :description, :string
      field :collected_at, :utc_datetime
    end

    # Status tracking
    field :status, Ecto.Enum,
      values: [:reported, :under_review, :panel_review, :resolved, :dismissed, :escalated],
      default: :reported

    field :priority, Ecto.Enum,
      values: [:low, :medium, :high, :critical],
      default: :medium

    # Timeline
    field :reported_at, :utc_datetime
    field :review_started_at, :utc_datetime
    field :panel_assigned_at, :utc_datetime
    field :resolved_at, :utc_datetime

    # Panel review (Clause 8.2)
    embeds_one :panel_review, PanelReview, on_replace: :update do
      field :assigned, :boolean, default: false
      field :panel_members, {:array, :string}
      field :licensor_representative, :string
      field :community_representatives, {:array, :string}
      field :review_date, :date
      field :findings, :string
      field :recommendation, :string
      field :vote_for, :integer
      field :vote_against, :integer
      field :vote_abstain, :integer
    end

    # Resolution
    embeds_one :resolution, Resolution, on_replace: :update do
      field :outcome, Ecto.Enum,
        values: [:violation_confirmed, :no_violation, :settled, :ongoing, :dismissed]

      field :outcome_description, :string
      field :remediation_required, {:array, :string}
      field :remediation_completed, :boolean, default: false
      field :remediation_date, :date
      field :compensation_offered, :boolean
      field :compensation_details, :string
      field :public_apology_required, :boolean
      field :public_apology_issued, :boolean
      field :archived_in_symbolic_defence, :boolean, default: false
      field :archive_uri, :string
    end

    # Legal action
    embeds_one :legal_action, LegalAction, on_replace: :update do
      field :initiated, :boolean, default: false
      field :jurisdiction, :string
      field :case_number, :string
      field :filing_date, :date
      field :status, :string
      field :outcome, :string
    end

    # Internal notes (not public)
    field :internal_notes, :string
    field :assigned_to, :string

    timestamps(type: :utc_datetime)
  end

  @doc """
  Changeset for creating a new violation report.
  """
  def changeset(violation, attrs) do
    violation
    |> cast(attrs, [
      :violation_id,
      :work_uri,
      :work_id,
      :work_title,
      :violation_type,
      :description,
      :detailed_explanation,
      :status,
      :priority,
      :reported_at,
      :review_started_at,
      :panel_assigned_at,
      :resolved_at,
      :internal_notes,
      :assigned_to
    ])
    |> cast_embed(:reported_by, with: &reporter_changeset/2, required: true)
    |> cast_embed(:alleged_violator, with: &alleged_violator_changeset/2)
    |> cast_embed(:evidence, with: &evidence_changeset/2)
    |> cast_embed(:panel_review, with: &panel_review_changeset/2)
    |> cast_embed(:resolution, with: &resolution_changeset/2)
    |> cast_embed(:legal_action, with: &legal_action_changeset/2)
    |> validate_required([
      :violation_id,
      :work_uri,
      :violation_type,
      :description
    ])
    |> validate_format(:violation_id, ~r/^violation-report-[a-z0-9-]+$/)
    |> validate_length(:description, min: 20, max: 5000)
    |> unique_constraint(:violation_id)
    |> set_reported_timestamp()
  end

  defp reporter_changeset(reporter, attrs) do
    reporter
    |> cast(attrs, [:name, :email, :uri, :relationship, :anonymous])
    |> validate_required([:relationship])
    |> validate_reporter_identity()
  end

  defp alleged_violator_changeset(violator, attrs) do
    violator
    |> cast(attrs, [
      :name,
      :organisation,
      :uri,
      :contact_attempted,
      :contact_date,
      :response_received
    ])
  end

  defp evidence_changeset(evidence, attrs) do
    evidence
    |> cast(attrs, [:type, :content, :uri, :ipfs_hash, :description, :collected_at])
    |> validate_required([:type])
    |> validate_evidence_content()
  end

  defp panel_review_changeset(panel, attrs) do
    panel
    |> cast(attrs, [
      :assigned,
      :panel_members,
      :licensor_representative,
      :community_representatives,
      :review_date,
      :findings,
      :recommendation,
      :vote_for,
      :vote_against,
      :vote_abstain
    ])
    |> validate_panel_composition()
  end

  defp resolution_changeset(resolution, attrs) do
    resolution
    |> cast(attrs, [
      :outcome,
      :outcome_description,
      :remediation_required,
      :remediation_completed,
      :remediation_date,
      :compensation_offered,
      :compensation_details,
      :public_apology_required,
      :public_apology_issued,
      :archived_in_symbolic_defence,
      :archive_uri
    ])
    |> validate_resolution_consistency()
  end

  defp legal_action_changeset(legal, attrs) do
    legal
    |> cast(attrs, [
      :initiated,
      :jurisdiction,
      :case_number,
      :filing_date,
      :status,
      :outcome
    ])
    |> validate_legal_action_details()
  end

  # Custom validators

  defp validate_reporter_identity(changeset) do
    anonymous = get_field(changeset, :anonymous)
    name = get_field(changeset, :name)
    email = get_field(changeset, :email)

    if !anonymous && (is_nil(name) || is_nil(email)) do
      changeset
      |> add_error(:name, "required unless anonymous")
      |> add_error(:email, "required unless anonymous")
    else
      changeset
    end
  end

  defp validate_evidence_content(changeset) do
    type = get_field(changeset, :type)
    content = get_field(changeset, :content)
    uri = get_field(changeset, :uri)

    if is_nil(content) && is_nil(uri) do
      add_error(changeset, :evidence, "must provide either content or URI")
    else
      changeset
    end
  end

  defp validate_panel_composition(changeset) do
    assigned = get_field(changeset, :assigned)
    community_reps = get_field(changeset, :community_representatives) || []

    if assigned && length(community_reps) != 2 do
      add_error(
        changeset,
        :community_representatives,
        "must have exactly 2 community representatives (Clause 8.2)"
      )
    else
      changeset
    end
  end

  defp validate_resolution_consistency(changeset) do
    outcome = get_field(changeset, :outcome)
    remediation = get_field(changeset, :remediation_required) || []

    if outcome == :violation_confirmed && Enum.empty?(remediation) do
      add_error(
        changeset,
        :remediation_required,
        "must specify remediation when violation is confirmed"
      )
    else
      changeset
    end
  end

  defp validate_legal_action_details(changeset) do
    initiated = get_field(changeset, :initiated)
    jurisdiction = get_field(changeset, :jurisdiction)
    case_number = get_field(changeset, :case_number)

    if initiated && (is_nil(jurisdiction) || is_nil(case_number)) do
      changeset
      |> add_error(:jurisdiction, "required when legal action initiated")
      |> add_error(:case_number, "required when legal action initiated")
    else
      changeset
    end
  end

  defp set_reported_timestamp(changeset) do
    case get_field(changeset, :reported_at) do
      nil -> put_change(changeset, :reported_at, DateTime.utc_now())
      _ -> changeset
    end
  end

  @doc """
  Updates violation status with appropriate timestamp.
  """
  def update_status(violation, new_status) do
    timestamp_field =
      case new_status do
        :under_review -> :review_started_at
        :panel_review -> :panel_assigned_at
        :resolved -> :resolved_at
        :dismissed -> :resolved_at
        _ -> nil
      end

    changeset =
      violation
      |> change()
      |> put_change(:status, new_status)

    if timestamp_field do
      put_change(changeset, timestamp_field, DateTime.utc_now())
    else
      changeset
    end
  end

  @doc """
  Returns whether this violation requires panel review.
  """
  def requires_panel_review?(%__MODULE__{} = violation) do
    violation.violation_type in [
      :cultural_misrepresentation,
      :trivialisation,
      :flattening
    ] || violation.priority == :critical
  end

  @doc """
  Returns whether remediation has been completed.
  """
  def remediation_complete?(%__MODULE__{resolution: nil}), do: false

  def remediation_complete?(%__MODULE__{resolution: resolution}),
    do: resolution.remediation_completed

  @doc """
  Calculates time elapsed since report.
  """
  def time_since_report(%__MODULE__{reported_at: reported_at}) do
    DateTime.diff(DateTime.utc_now(), reported_at, :day)
  end
end
