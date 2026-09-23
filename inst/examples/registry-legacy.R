# For existing schema 2 callers only; the constructor warns about deprecation.
entry <- suppressWarnings(brief_plot_registry_entry(
  id = "legacy-board", title = "Legacy example", collection = "macro",
  asset_class = "Rates", indicator_family = "Yield Curve", region = "United States",
  frequency = "Daily", source = "Synthetic data", last_updated = "2025-01-07",
  status = "ready", plot_image = "plots/example.svg",
  thumbnail = "thumbnails/example.png", tags = "example"
))
entry$last_updated
