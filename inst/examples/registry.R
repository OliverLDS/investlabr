data_date <- brief_data_as_of(list(
  daily = as.Date(c("2025-01-06", "2025-01-07")),
  weekly = as.Date(c("2025-01-01", "2025-01-08"))
))
entry <- brief_plot_registry_entry_v3(
  id = "example-board", title = "Illustrative research board", collection = "macro",
  asset_class = "Rates", indicator_family = "Yield Curve", region = "United States",
  frequency = "Daily", source = "Synthetic data", expected_cadence = "weekly",
  rendered_at = "2025-01-09T12:00:00Z", data_as_of = data_date,
  metadata_updated_at = "2025-01-01", status = "ready",
  plot_image = "plots/example.svg", thumbnail = "thumbnails/example.png", tags = "example"
)
# This example validates metadata without requiring image files.
path <- tempfile(fileext = ".json")
brief_plot_registry_write(list(entry), path, schema_version = "3.0",
                           generated_at = "2025-01-09T12:01:00Z")
brief_plot_registry_validate(path, require_assets = FALSE)
unlink(path)
