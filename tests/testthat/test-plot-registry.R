registry_test_entry <- function(id = "us-yield-curve-10y-2y", status = "ready") {
  brief_plot_registry_entry(
    id = id,
    title = "US 10Y-2Y Yield Curve",
    collection = "macro",
    asset_class = "Rates",
    indicator_family = "Yield Curve",
    region = "United States",
    frequency = "Daily",
    source = "FRED",
    last_updated = as.Date("2026-04-30"),
    status = status,
    plot_image = paste0("plots/macro/", id, ".svg"),
    thumbnail = paste0("thumbnails/macro/", id, ".png"),
    tags = c("rates", "yield curve")
  )
}

test_that("schema 2.0 entries validate and exclude consumer fields", {
  entry <- registry_test_entry()

  expect_identical(entry$collection, "macro")
  expect_identical(entry$status, "ready")
  expect_identical(entry$last_updated, "2026-04-30")
  expect_identical(entry$plot_html, "")
  expect_identical(entry$curation_priority, 0L)
  expect_true(is.list(entry$compliance))
  expect_false(any(c(
    "dashboard", "published", "featured", "canonical_slug",
    "podcast_topics", "report_topics"
  ) %in% names(entry)))
})

test_that("entries reject invalid controlled values and unsafe paths", {
  expect_error(
    brief_plot_registry_entry(
      id = "BadId", title = "Bad", collection = "macro",
      asset_class = "Rates", indicator_family = "Yield Curve",
      region = "United States", frequency = "Daily", source = "FRED",
      last_updated = "2026-04-30", status = "ready",
      plot_image = "plots/macro/bad.svg",
      thumbnail = "thumbnails/macro/bad.png", tags = "rates"
    ),
    "kebab-case"
  )
  expect_error(
    brief_plot_registry_entry(
      id = "good-id", title = "Bad", collection = "macro",
      asset_class = "Unknown", indicator_family = "Yield Curve",
      region = "United States", frequency = "Daily", source = "FRED",
      last_updated = "2026-04-30", status = "ready",
      plot_image = "plots/macro/good.svg",
      thumbnail = "thumbnails/macro/good.png", tags = "rates"
    ),
    "asset_class"
  )
  expect_error(
    brief_plot_registry_entry(
      id = "unsafe-path", title = "Bad path", collection = "macro",
      asset_class = "Rates", indicator_family = "Validation",
      region = "United States", frequency = "Daily", source = "FRED",
      last_updated = "2026-04-30", status = "ready",
      plot_image = "../bad.svg",
      thumbnail = "thumbnails/macro/good.png", tags = "rates"
    ),
    "must not traverse"
  )
})

test_that("schema 2.0 sidecars build a ready-only registry", {
  root <- file.path(tempdir(), "publishing-registry-test")
  unlink(root, recursive = TRUE)
  meta_dir <- file.path(root, "meta")
  dir.create(file.path(root, "plots", "macro"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(root, "thumbnails", "macro"), recursive = TRUE, showWarnings = FALSE)
  dir.create(meta_dir, recursive = TRUE, showWarnings = FALSE)

  writeLines("svg", file.path(root, "plots", "macro", "fed-reserves.svg"))
  writeLines("png", file.path(root, "thumbnails", "macro", "fed-reserves.png"))
  writeLines(
    c(
      "id: fed-reserves",
      "title: Fed Reserves",
      "collection: macro",
      "asset_class: Liquidity",
      "indicator_family: Fed Balance Sheet",
      "region: United States",
      "frequency: Weekly",
      "source: Federal Reserve",
      "last_updated: '2026-04-30'",
      "status: ready",
      "curation_priority: 50",
      "plot_image: plots/macro/fed-reserves.svg",
      "thumbnail: thumbnails/macro/fed-reserves.png",
      "tags:",
      "  - liquidity",
      "  - reserves"
    ),
    file.path(meta_dir, "fed-reserves.yaml")
  )

  out_path <- file.path(root, "plot-registry.json")
  reg <- brief_plot_registry_write_from_meta(
    meta_dir = meta_dir,
    path = out_path,
    output_root = root,
    ready_only = TRUE
  )

  expect_identical(reg$schema_version, "2.0")
  expect_length(reg$plots, 1L)
  parsed <- jsonlite::fromJSON(out_path, simplifyVector = FALSE)
  expect_identical(parsed$plots[[1]]$collection, "macro")
  expect_identical(parsed$plots[[1]]$status, "ready")
  expect_false("canonical_slug" %in% names(parsed$plots[[1]]))

  validation <- brief_plot_registry_validate(out_path, output_root = root)
  expect_true(validation$valid)
  expect_identical(validation$plot_count, 1L)
  expect_identical(validation$ready_count, 1L)
})

test_that("ready-only writing excludes draft and archived entries", {
  path <- tempfile(fileext = ".json")
  reg <- brief_plot_registry_write(
    plots = list(
      registry_test_entry("ready-plot", "ready"),
      registry_test_entry("draft-plot", "draft"),
      registry_test_entry("archived-plot", "archived")
    ),
    path = path,
    ready_only = TRUE
  )
  expect_identical(vapply(reg$plots, `[[`, character(1), "id"), "ready-plot")
})

test_that("schema 1.0 registries normalize during validation", {
  legacy_entry <- list(
    id = "legacy-plot", title = "Legacy plot", dashboard = "macro",
    asset_class = "Rates", indicator_family = "Validation",
    region = "United States", frequency = "Daily", source = "FRED",
    last_updated = "2026-04-30", published = TRUE,
    plot_image = "plots/macro/legacy.svg",
    thumbnail = "thumbnails/macro/legacy.png", tags = list("legacy")
  )
  registry <- list(
    schema_version = "1.0",
    generated_at = "2026-04-30T00:00:00Z",
    source_system = "investlabr",
    base_path_mode = "relative",
    plots = list(legacy_entry)
  )
  validation <- brief_plot_registry_validate(registry, require_assets = FALSE)
  expect_identical(validation$schema_version, "1.0")
  expect_identical(validation$ready_count, 1L)
})

test_that("schema 2.0 rejects legacy and unknown consumer fields", {
  legacy_entry <- registry_test_entry("legacy-in-v2")
  legacy_entry$dashboard <- "macro"
  legacy_registry <- list(
    schema_version = "2.0",
    generated_at = "2026-04-30T00:00:00Z",
    source_system = "investlabr",
    base_path_mode = "relative",
    plots = list(legacy_entry)
  )
  expect_error(
    brief_plot_registry_validate(legacy_registry, require_assets = FALSE),
    "legacy consumer field"
  )

  unknown_entry <- registry_test_entry("unknown-in-v2")
  unknown_entry$website_route <- "/macro/example/"
  unknown_registry <- legacy_registry
  unknown_registry$plots <- list(unknown_entry)
  expect_error(
    brief_plot_registry_validate(unknown_registry, require_assets = FALSE),
    "unknown field"
  )
})

test_that("registry validation rejects unsupported schema versions", {
  registry <- list(
    schema_version = "3.0",
    generated_at = "2026-04-30T00:00:00Z",
    source_system = "investlabr",
    base_path_mode = "relative",
    plots = list()
  )
  expect_error(
    brief_plot_registry_validate(registry, require_assets = FALSE),
    "1.0.*2.0"
  )
})

test_that("legacy entry arguments warn and return schema 2.0 fields", {
  expect_warning(
    entry <- brief_plot_registry_entry(
      id = "legacy-call", title = "Legacy call", dashboard = "macro",
      asset_class = "Rates", indicator_family = "Validation",
      region = "United States", frequency = "Daily", source = "FRED",
      last_updated = "2026-04-30", published = TRUE,
      plot_image = "plots/macro/legacy.svg",
      thumbnail = "thumbnails/macro/legacy.png", tags = "legacy"
    ),
    "deprecated"
  )
  expect_identical(entry$collection, "macro")
  expect_identical(entry$status, "ready")
  expect_false("published" %in% names(entry))
})

test_that("registry validation rejects duplicate ids and missing assets", {
  entry <- registry_test_entry("duplicate")
  duplicate_registry <- list(
    schema_version = "2.0",
    generated_at = "2026-04-30T00:00:00Z",
    source_system = "investlabr",
    base_path_mode = "relative",
    plots = list(entry, entry)
  )
  expect_error(
    brief_plot_registry_validate(duplicate_registry, require_assets = FALSE),
    "unique"
  )

  missing_registry <- duplicate_registry
  missing_registry$plots <- list(entry)
  expect_error(
    brief_plot_registry_validate(missing_registry, output_root = tempdir()),
    "does not exist"
  )
})
