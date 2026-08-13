registry_v3_entry <- function(
  id = "us-yield-curve-10y-2y", status = "ready", time_indexed = TRUE,
  rendered_at = "2026-08-05T09:15:00Z", data_as_of = "2026-08-04",
  expected_cadence = if (time_indexed) "daily" else "not_time_indexed"
) {
  brief_plot_registry_entry_v3(
    id = id,
    title = "US 10Y-2Y Yield Curve",
    collection = "macro",
    asset_class = "Rates",
    indicator_family = "Yield Curve",
    region = "United States",
    frequency = "Daily",
    source = "FRED",
    rendered_at = rendered_at,
    data_as_of = data_as_of,
    metadata_updated_at = "2026-08-05",
    time_indexed = time_indexed,
    expected_cadence = expected_cadence,
    status = status,
    plot_image = paste0("plots/macro/", id, ".svg"),
    thumbnail = paste0("thumbnails/macro/", id, ".png"),
    tags = c("rates", "yield curve")
  )
}

find_publishing_repo_root <- function() {
  starts <- unique(c(getwd(), testthat::test_path(), dirname(testthat::test_path())))
  for (start in starts) {
    current <- normalizePath(start, mustWork = FALSE)
    repeat {
      if (dir.exists(file.path(current, "config", "publishing", "plots"))) return(current)
      parent <- dirname(current)
      if (identical(parent, current)) break
      current <- parent
    }
  }
  NULL
}

registry_object <- function(entry, schema = "3.0", generated_at = "2026-08-05T09:16:00Z") {
  list(
    schema_version = schema,
    generated_at = generated_at,
    source_system = "investlabr",
    base_path_mode = "relative",
    plots = list(entry)
  )
}

write_sidecar_fixture <- function(
  root, resolved_metadata_updated_at = "2026-08-05",
  resolved_time_indexed = TRUE, resolved_expected_cadence = "daily"
) {
  dir.create(file.path(root, "meta"), recursive = TRUE)
  dir.create(file.path(root, "resolved"), recursive = TRUE)
  dir.create(file.path(root, "plots", "macro"), recursive = TRUE)
  dir.create(file.path(root, "thumbnails", "macro"), recursive = TRUE)
  writeLines("svg", file.path(root, "plots", "macro", "test.svg"))
  writeLines("png", file.path(root, "thumbnails", "macro", "test.png"))
  sidecar <- file.path(root, "meta", "test.yaml")
  writeLines(c(
    "id: test-artifact", "title: Test artifact", "collection: macro",
    "asset_class: Rates", "indicator_family: Test", "region: United States",
    "frequency: Daily", "source: FRED", "metadata_updated_at: '2026-08-05'",
    "time_indexed: true", "expected_cadence: daily", "status: ready", "plot_image: plots/macro/test.svg",
    "thumbnail: thumbnails/macro/test.png", "tags:", "  - test"
  ), sidecar)
  jsonlite::write_json(list(
    id = "test-artifact", rendered_at = "2026-08-05T00:00:00Z",
    data_as_of = "2026-08-04",
    metadata_updated_at = resolved_metadata_updated_at,
    time_indexed = resolved_time_indexed,
    expected_cadence = resolved_expected_cadence,
    data_as_of_rule = "test conservative rule"
  ), file.path(root, "resolved", "test-artifact.json"), auto_unbox = TRUE)
  sidecar
}

test_that("schema 3.0 entries contain distinct publishing dates", {
  entry <- registry_v3_entry()
  expect_identical(entry$rendered_at, "2026-08-05T09:15:00Z")
  expect_identical(entry$data_as_of, "2026-08-04")
  expect_identical(entry$metadata_updated_at, "2026-08-05")
  expect_true(entry$time_indexed)
  expect_identical(entry$expected_cadence, "daily")
  expect_false("last_updated" %in% names(entry))
  expect_true(brief_plot_registry_validate(registry_object(entry), require_assets = FALSE)$valid)
})

test_that("deprecated schema 2.0 constructor calls remain writable", {
  expect_warning(
    legacy <- brief_plot_registry_entry(
      id = "legacy-call", title = "Legacy call", dashboard = "macro",
      asset_class = "Rates", indicator_family = "Compatibility",
      region = "United States", frequency = "Daily", source = "FRED",
      last_updated = "2026-04-30", published = TRUE, sort_priority = 42,
      featured = TRUE, canonical_slug = "legacy-call",
      podcast_topics = "Rates", report_topics = "Daily Macro Brief",
      plot_image = "plots/macro/legacy-call.svg",
      thumbnail = "thumbnails/macro/legacy-call.png", tags = "legacy"
    ),
    "deprecated"
  )
  expect_identical(legacy$collection, "macro")
  expect_identical(legacy$status, "ready")
  expect_identical(legacy$curation_priority, 42L)
  expect_identical(legacy$last_updated, "2026-04-30")
  expect_false(any(c("rendered_at", "metadata_updated_at") %in% names(legacy)))

  path <- tempfile(fileext = ".json")
  v2 <- brief_plot_registry_write(list(legacy), path, schema_version = "2.0")
  expect_identical(v2$plots[[1]]$last_updated, "2026-04-30")
  expect_error(
    brief_plot_registry_write(list(legacy), tempfile(fileext = ".json"), schema_version = "3.0"),
    "cannot be emitted as schema 3.0"
  )
})

test_that("schema 3.0 forbids last_updated and unknown fields", {
  entry <- registry_v3_entry()
  entry$last_updated <- "2026-08-04"
  expect_error(
    brief_plot_registry_validate(registry_object(entry), require_assets = FALSE),
    "forbids `last_updated`"
  )
  entry$last_updated <- NULL
  entry$website_route <- "/macro/example"
  expect_error(
    brief_plot_registry_validate(registry_object(entry), require_assets = FALSE),
    "unknown field"
  )
})

test_that("schema 3.0 rejects malformed dates and timestamps", {
  expect_error(registry_v3_entry(rendered_at = "2026-08-05 09:15:00"), "UTC timestamp")
  expect_error(registry_v3_entry(rendered_at = "2026-02-30T09:15:00Z"), "UTC timestamp")
  expect_error(registry_v3_entry(data_as_of = "08/04/2026"), "valid ISO date")
  expect_error(
    brief_plot_registry_entry_v3(
      id = "bad-metadata-date", title = "Bad", collection = "macro",
      asset_class = "Rates", indicator_family = "Test", region = "United States",
      frequency = "Daily", source = "FRED", rendered_at = "2026-08-05T09:15:00Z",
      data_as_of = "2026-08-04", metadata_updated_at = "2026-02-30", expected_cadence = "daily",
      status = "ready", plot_image = "plots/macro/a.svg",
      thumbnail = "thumbnails/macro/a.png", tags = "test"
    ),
    "valid ISO date"
  )
})

test_that("ready and draft artifacts enforce appropriate runtime fields", {
  expect_error(registry_v3_entry(rendered_at = NULL), "require non-null `rendered_at`")
  expect_error(registry_v3_entry(data_as_of = NULL), "require non-null `data_as_of`")
  draft <- registry_v3_entry(
    status = "draft", time_indexed = FALSE, rendered_at = NULL, data_as_of = NULL
  )
  expect_null(draft$rendered_at)
  expect_null(draft$data_as_of)
  ready_non_time <- registry_v3_entry(time_indexed = FALSE, data_as_of = NULL)
  expect_null(ready_non_time$data_as_of)
})

test_that("schema 3.0 requires a constrained expected cadence", {
  expect_error(
    brief_plot_registry_entry_v3(
      id = "missing-cadence", title = "Missing", collection = "macro",
      asset_class = "Rates", indicator_family = "Test", region = "United States",
      frequency = "Daily", source = "FRED", rendered_at = "2026-08-05T09:15:00Z",
      data_as_of = "2026-08-04", metadata_updated_at = "2026-08-05",
      time_indexed = TRUE, status = "ready", plot_image = "plots/macro/a.svg",
      thumbnail = "thumbnails/macro/a.png", tags = "test"
    ),
    "missing"
  )
  expect_error(registry_v3_entry(expected_cadence = "hourly"), "expected_cadence")
  for (cadence in c("daily", "weekly", "monthly", "event_driven")) {
    expect_identical(registry_v3_entry(expected_cadence = cadence)$expected_cadence, cadence)
  }
  non_time <- registry_v3_entry(
    time_indexed = FALSE, data_as_of = NULL, expected_cadence = "not_time_indexed"
  )
  expect_identical(non_time$expected_cadence, "not_time_indexed")
  expect_error(
    registry_v3_entry(expected_cadence = "not_time_indexed"),
    "not_time_indexed.*time_indexed"
  )
  expect_error(
    registry_v3_entry(time_indexed = FALSE, expected_cadence = "weekly"),
    "Time-indexed expected cadences"
  )
})

test_that("publishing dates cannot postdate rendering", {
  expect_error(registry_v3_entry(data_as_of = "2026-08-06"), "cannot be later")
  expect_error(
    registry_v3_entry(rendered_at = "2026-08-04T23:59:59Z"),
    "metadata_updated_at.*cannot be later"
  )
})

test_that("brief_data_as_of uses the stalest required latest date", {
  result <- brief_data_as_of(list(
    daily = data.frame(date = as.Date(c("2026-08-03", "2026-08-04"))),
    weekly = data.frame(date = as.Date(c("2026-07-24", "2026-07-31"))),
    monthly = as.Date(c("2026-06-30", "2026-07-31"))
  ))
  expect_identical(result, "2026-07-31")
})

test_that("Yahoo current-date bars can be conservatively excluded", {
  yahoo_raw <- data.frame(
    date = as.Date(c("2026-08-03", "2026-08-04", "2026-08-05")),
    close = c(100, 101, NA_real_)
  )
  yahoo_consumed <- yahoo_raw[is.finite(yahoo_raw$close), , drop = FALSE]
  result <- brief_data_as_of(
    list(
      NYSE = yahoo_consumed,
      Tokyo = as.Date(c("2026-08-01", "2026-08-04"))
    ),
    completed_before = "2026-08-05"
  )
  expect_identical(result, "2026-08-04")
  expect_error(
    brief_data_as_of(list(empty = as.Date("2026-08-05")), completed_before = "2026-08-05"),
    "no usable observation"
  )
})

test_that("schema 2.0 compatibility maps last_updated from data_as_of", {
  path <- tempfile(fileext = ".json")
  reg <- brief_plot_registry_write(
    plots = list(registry_v3_entry()), path = path, schema_version = "2.0"
  )
  emitted <- reg$plots[[1]]
  expect_identical(emitted$last_updated, "2026-08-04")
  expect_false(any(c("rendered_at", "data_as_of", "metadata_updated_at", "time_indexed", "expected_cadence") %in% names(emitted)))
  expect_true(brief_plot_registry_validate(path, require_assets = FALSE)$valid)
})

test_that("schema 3.0 writer emits scalar deterministic JSON", {
  path <- tempfile(fileext = ".json")
  brief_plot_registry_write(
    plots = list(registry_v3_entry()), path = path, schema_version = "3.0",
    generated_at = "2026-08-05T09:16:00Z"
  )
  parsed <- jsonlite::read_json(path, simplifyVector = FALSE)
  expect_type(parsed$plots[[1]]$rendered_at, "character")
  expect_type(parsed$plots[[1]]$time_indexed, "logical")
  expect_identical(parsed$plots[[1]]$expected_cadence, "daily")
  expect_type(parsed$plots[[1]]$tags, "list")
  expect_identical(parsed$plots[[1]]$tags, list("rates", "yield curve"))
})

test_that("schema 1.0 and 2.0 remain readable", {
  v1 <- list(
    id = "legacy-plot", title = "Legacy", dashboard = "macro",
    asset_class = "Rates", indicator_family = "Test", region = "United States",
    frequency = "Daily", source = "FRED", last_updated = "2026-04-30",
    published = TRUE, plot_image = "plots/macro/a.svg",
    thumbnail = "thumbnails/macro/a.png", tags = list("legacy")
  )
  expect_true(brief_plot_registry_validate(registry_object(v1, "1.0"), require_assets = FALSE)$valid)

  v2 <- registry_v3_entry()
  v2$last_updated <- v2$data_as_of
  v2[c("rendered_at", "data_as_of", "metadata_updated_at", "time_indexed", "expected_cadence")] <- NULL
  expect_true(brief_plot_registry_validate(registry_object(v2, "2.0"), require_assets = FALSE)$valid)
})

test_that("registry generation follows ready artifact rendering", {
  entry <- registry_v3_entry()
  expect_error(
    brief_plot_registry_validate(
      registry_object(entry, generated_at = "2026-08-05T09:14:59Z"),
      require_assets = FALSE
    ),
    "generated_at.*later"
  )
})

test_that("runtime rendered_at is generated after artifact creation", {
  before <- Sys.time()
  path <- tempfile(fileext = ".json")
  entry <- registry_v3_entry(rendered_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"))
  brief_plot_registry_write(list(entry), path, schema_version = "3.0")
  after <- Sys.time()
  stamp <- as.POSIXct(entry$rendered_at, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  expect_gte(stamp, as.POSIXct(format(before, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"), tz = "UTC"))
  expect_lte(stamp, after)
})

test_that("sidecars join run-local resolved metadata without being rewritten", {
  root <- file.path(tempdir(), paste0("registry-sidecar-", Sys.getpid()))
  unlink(root, recursive = TRUE)
  sidecar <- write_sidecar_fixture(root)
  hash_before <- tools::md5sum(sidecar)
  out <- file.path(root, "registry.json")
  reg <- brief_plot_registry_write_from_meta(
    meta_dir = file.path(root, "meta"), path = out, output_root = root,
    resolved_meta_dir = file.path(root, "resolved"), schema_version = "3.0"
  )
  expect_identical(reg$plots[[1]]$data_as_of, "2026-08-04")
  expect_identical(unname(tools::md5sum(sidecar)), unname(hash_before))
})

test_that("sidecar joins reject stale metadata_updated_at", {
  root <- file.path(tempdir(), paste0("registry-meta-mismatch-", Sys.getpid()))
  unlink(root, recursive = TRUE)
  write_sidecar_fixture(root, resolved_metadata_updated_at = "2026-08-04")
  expect_error(
    brief_plot_registry_write_from_meta(
      meta_dir = file.path(root, "meta"), path = file.path(root, "registry.json"),
      output_root = root, resolved_meta_dir = file.path(root, "resolved"),
      schema_version = "3.0"
    ),
    "stale relative to tracked metadata"
  )
})

test_that("sidecar joins reject stale time_indexed", {
  root <- file.path(tempdir(), paste0("registry-time-mismatch-", Sys.getpid()))
  unlink(root, recursive = TRUE)
  write_sidecar_fixture(root, resolved_time_indexed = FALSE)
  expect_error(
    brief_plot_registry_write_from_meta(
      meta_dir = file.path(root, "meta"), path = file.path(root, "registry.json"),
      output_root = root, resolved_meta_dir = file.path(root, "resolved"),
      schema_version = "3.0"
    ),
    "`time_indexed` is stale"
  )
})

test_that("sidecar joins reject stale expected_cadence", {
  root <- file.path(tempdir(), paste0("registry-cadence-mismatch-", Sys.getpid()))
  unlink(root, recursive = TRUE)
  write_sidecar_fixture(root, resolved_expected_cadence = "weekly")
  expect_error(
    brief_plot_registry_write_from_meta(
      meta_dir = file.path(root, "meta"), path = file.path(root, "registry.json"),
      output_root = root, resolved_meta_dir = file.path(root, "resolved"),
      schema_version = "3.0"
    ),
    "`expected_cadence` is stale"
  )
})

test_that("all ready publishing sidecars declare valid schema 3 cadences", {
  repo <- find_publishing_repo_root()
  skip_if(is.null(repo), "Tracked publishing sidecars are not installed with the package.")
  sidecar_paths <- sort(Sys.glob(file.path(repo, "config", "publishing", "plots", "*.yaml")))
  sidecars <- lapply(sidecar_paths, yaml::read_yaml)
  ready <- Filter(function(x) identical(x$status, "ready"), sidecars)
  expect_length(ready, 7L)
  expected <- c(
    "fred-balance-sheet-mirror-board" = "weekly",
    "fred-fomc-plumbing-board" = "weekly",
    "fred-inflation-labor-dashboard" = "monthly",
    "fred-liquidity-tightness-dashboard" = "weekly",
    "fred-rate-shock-persistence-board" = "daily",
    "macro-factor-heatmap" = "daily",
    "yahoo-cross-asset-event-board" = "daily"
  )
  observed <- vapply(ready, `[[`, character(1), "expected_cadence")
  names(observed) <- vapply(ready, `[[`, character(1), "id")
  expect_identical(observed[sort(names(expected))], expected[sort(names(expected))])
  for (sidecar in ready) {
    entry <- .brief_registry_v3_from_list(c(
      sidecar,
      list(rendered_at = "2026-08-13T09:15:00Z", data_as_of = "2026-08-04")
    ))
    expect_true(
      brief_plot_registry_validate(
        registry_object(entry, generated_at = "2026-08-13T09:16:00Z"),
        require_assets = FALSE
      )$valid
    )
  }
})

test_that("preview rendering does not modify tracked metadata", {
  repo <- find_publishing_repo_root()
  skip_if(is.null(repo), "Rendering node is not installed with the package.")
  script <- file.path(repo, "scripts", "render_plot_assets.R")
  skip_if_not(file.exists(script))
  sidecars <- sort(Sys.glob(file.path(repo, "config", "publishing", "plots", "*.yaml")))
  before <- tools::md5sum(sidecars)
  output <- file.path(tempdir(), paste0("renderer-preview-", Sys.getpid()))
  unlink(output, recursive = TRUE)
  status <- system2(
    file.path(R.home("bin"), "Rscript"),
    c(script, "--ids", "context-report", "--output-root", output, "--repo-root", repo),
    stdout = TRUE, stderr = TRUE
  )
  expect_false(isTRUE(attr(status, "status") != 0L), info = paste(status, collapse = "\n"))
  expect_identical(unname(tools::md5sum(sidecars)), unname(before))
  resolved <- jsonlite::read_json(file.path(output, "resolved", "context-report.json"), simplifyVector = FALSE)
  expect_null(resolved$data_as_of)
  expect_identical(resolved$expected_cadence, "not_time_indexed")
  expect_match(resolved$rendered_at, "Z$")
})
