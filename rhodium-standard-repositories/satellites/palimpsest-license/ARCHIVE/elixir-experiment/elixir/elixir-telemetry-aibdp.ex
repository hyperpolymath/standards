# Telemetry example:
:telemetry.attach("aibdp_manifest", [:palimpsest, :manifest, :request], fn _event, %{format: format}, _metadata ->
  StatsD.increment("aibdp.manifest_served.#{format}")
end)
