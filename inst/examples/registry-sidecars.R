local({
  root <- tempfile("registry-example-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE))
  dir.create(file.path(root, "meta"))
  dir.create(file.path(root, "resolved"))
  # Use actual temporary plot artifacts, with explicit synthetic metadata.
  pdf_path <- file.path(root, "example.pdf")
  grDevices::pdf(pdf_path)
  plot(1:3)
  grDevices::dev.off()
  tracked <- list(
    id = "example-board", title = "Example", collection = "macro",
    asset_class = "Rates", indicator_family = "Example", region = "Global",
    frequency = "Event", source = "Synthetic data", status = "ready",
    metadata_updated_at = "2025-01-01", time_indexed = FALSE,
    expected_cadence = "not_time_indexed", plot_image = "example.pdf",
    thumbnail = "example.pdf", tags = "example"
  )
  yaml::write_yaml(tracked, file.path(root, "meta", "example.yaml"))
  jsonlite::write_json(list(
    id = tracked$id, rendered_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    data_as_of = NULL, metadata_updated_at = tracked$metadata_updated_at,
    time_indexed = FALSE, expected_cadence = "not_time_indexed",
    data_as_of_rule = "not_time_indexed"
  ), file.path(root, "resolved", "example-board.json"), auto_unbox = TRUE, null = "null")
  brief_plot_registry_write_from_meta(
    file.path(root, "meta"), file.path(root, "registry.json"), output_root = root,
    schema_version = "3.0"
  )
})
