test_that("brief_plot_registry_entry validates and normalizes fields", {
  entry <- brief_plot_registry_entry(
    id = "us-yield-curve-10y-2y",
    title = "US 10Y-2Y Yield Curve",
    dashboard = "macro",
    asset_class = "Rates",
    indicator_family = "Yield Curve",
    region = "United States",
    frequency = "Daily",
    source = "FRED",
    last_updated = as.Date("2026-04-30"),
    published = TRUE,
    plot_image = "plots/macro/us-yield-curve-10y-2y.svg",
    thumbnail = "thumbnails/macro/us-yield-curve-10y-2y.png",
    tags = c("rates", "yield curve")
  )

  expect_identical(entry$id, "us-yield-curve-10y-2y")
  expect_identical(entry$canonical_slug, "us-yield-curve-10y-2y")
  expect_identical(entry$last_updated, "2026-04-30")
  expect_identical(entry$plot_html, "")
  expect_true(is.list(entry$compliance))
  expect_true(isTRUE(entry$compliance$educational_only))
})

test_that("brief_plot_registry_entry rejects invalid controlled values", {
  expect_error(
    brief_plot_registry_entry(
      id = "BadId",
      title = "Bad",
      dashboard = "macro",
      asset_class = "Rates",
      indicator_family = "Yield Curve",
      region = "United States",
      frequency = "Daily",
      source = "FRED",
      last_updated = "2026-04-30",
      published = TRUE,
      plot_image = "plots/macro/bad.svg",
      thumbnail = "thumbnails/macro/bad.png",
      tags = "rates"
    ),
    "kebab-case"
  )

  expect_error(
    brief_plot_registry_entry(
      id = "good-id",
      title = "Bad",
      dashboard = "macro",
      asset_class = "Unknown",
      indicator_family = "Yield Curve",
      region = "United States",
      frequency = "Daily",
      source = "FRED",
      last_updated = "2026-04-30",
      published = TRUE,
      plot_image = "plots/macro/good.svg",
      thumbnail = "thumbnails/macro/good.png",
      tags = "rates"
    ),
    "asset_class"
  )
})

test_that("brief_plot_registry_write_from_meta builds registry from YAML sidecars", {
  root <- file.path(tempdir(), "alphasync-registry-test")
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
      "dashboard: macro",
      "asset_class: Liquidity",
      "indicator_family: Fed Balance Sheet",
      "region: United States",
      "frequency: Weekly",
      "source: Federal Reserve",
      "last_updated: '2026-04-30'",
      "published: true",
      "plot_image: plots/macro/fed-reserves.svg",
      "thumbnail: thumbnails/macro/fed-reserves.png",
      "tags:",
      "  - liquidity",
      "  - reserves"
    ),
    file.path(meta_dir, "fed-reserves.yaml")
  )

  out_path <- file.path(root, "alphasync-plot-registry.json")
  reg <- brief_plot_registry_write_from_meta(
    meta_dir = meta_dir,
    path = out_path,
    output_root = root,
    published_only = TRUE
  )

  expect_true(file.exists(out_path))
  expect_length(reg$plots, 1L)
  parsed <- jsonlite::fromJSON(out_path, simplifyVector = FALSE)
  expect_identical(parsed$plots[[1]]$id, "fed-reserves")
  expect_identical(parsed$plots[[1]]$plot_image, "plots/macro/fed-reserves.svg")
})
