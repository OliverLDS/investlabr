#' Build one research-artifact plot-registry entry
#'
#' @param id Stable machine-safe plot identifier in lowercase kebab-case.
#' @param title Reader-facing plot title.
#' @param collection High-level research collection. One of \code{"macro"},
#'   \code{"markets"}, \code{"crypto"}, or \code{"strategies"}.
#' @param asset_class Asset-class label such as \code{"Rates"} or
#'   \code{"Liquidity"}.
#' @param indicator_family Indicator family label such as \code{"Yield Curve"}.
#' @param region Region label.
#' @param frequency Frequency label.
#' @param source Source label such as \code{"FRED"}.
#' @param last_updated ISO date string or \code{Date}.
#' @param status Artifact readiness. One of \code{"draft"}, \code{"ready"}, or
#'   \code{"archived"}.
#' @param plot_image Relative path to the rendered primary plot image.
#' @param thumbnail Relative path to the thumbnail image.
#' @param tags Character vector of flat tags.
#' @param subtitle Optional subtitle.
#' @param summary Optional short summary.
#' @param description_md Optional longer markdown description.
#' @param section Optional narrower grouping such as \code{"rates"} or
#'   \code{"liquidity"}.
#' @param source_detail Optional character vector of source-level details such as
#'   series ids.
#' @param curation_priority Numeric research-curation priority.
#' @param plot_html Relative path to an optional HTML artifact. Use \code{""}
#'   when none exists.
#' @param related_ids Optional character vector of related plot ids.
#' @param compliance Named list. Defaults to educational-only and
#'   not-investment-advice flags.
#' @param dashboard,published,featured,sort_priority,canonical_slug,podcast_topics,report_topics
#'   Deprecated schema 1.0 arguments retained temporarily for source
#'   compatibility. New registries do not emit these fields.
#'
#' @return Named list representing one schema 2.0 registry entry.
#' @export
brief_plot_registry_entry <- function(
  id,
  title,
  collection = NULL,
  asset_class,
  indicator_family,
  region,
  frequency,
  source,
  last_updated,
  status = NULL,
  plot_image,
  thumbnail,
  tags,
  subtitle = "",
  summary = "",
  description_md = "",
  section = "",
  source_detail = character(),
  curation_priority = 0,
  plot_html = "",
  related_ids = character(),
  compliance = list(
    educational_only = TRUE,
    not_investment_advice = TRUE
  ),
  dashboard = NULL,
  published = NULL,
  featured = NULL,
  sort_priority = NULL,
  canonical_slug = NULL,
  podcast_topics = NULL,
  report_topics = NULL
) {
  legacy_fields <- names(Filter(
    Negate(is.null),
    list(
      dashboard = dashboard,
      published = published,
      featured = featured,
      sort_priority = sort_priority,
      canonical_slug = canonical_slug,
      podcast_topics = podcast_topics,
      report_topics = report_topics
    )
  ))
  if (length(legacy_fields) > 0L) {
    .brief_registry_warn_legacy(legacy_fields)
  }

  if (is.null(collection)) collection <- dashboard
  if (is.null(status) && !is.null(published)) {
    status <- if (isTRUE(published)) "ready" else "draft"
  }
  if (!is.null(sort_priority) && identical(curation_priority, 0)) {
    curation_priority <- sort_priority
  }

  entry <- list(
    id = .brief_registry_scalar_chr(id),
    title = .brief_registry_scalar_chr(title),
    subtitle = .brief_registry_scalar_chr(subtitle),
    summary = .brief_registry_scalar_chr(summary),
    description_md = .brief_registry_scalar_chr(description_md),
    collection = .brief_registry_scalar_chr(collection),
    section = .brief_registry_scalar_chr(section),
    asset_class = .brief_registry_scalar_chr(asset_class),
    indicator_family = .brief_registry_scalar_chr(indicator_family),
    region = .brief_registry_scalar_chr(region),
    frequency = .brief_registry_scalar_chr(frequency),
    source = .brief_registry_scalar_chr(source),
    source_detail = .brief_registry_chr(source_detail),
    tags = .brief_registry_chr(tags),
    last_updated = .brief_registry_format_date(last_updated),
    status = .brief_registry_scalar_chr(status),
    curation_priority = as.integer(curation_priority),
    plot_image = .brief_registry_normalize_rel_path(plot_image),
    thumbnail = .brief_registry_normalize_rel_path(thumbnail),
    plot_html = .brief_registry_normalize_rel_path(plot_html, allow_empty = TRUE),
    related_ids = .brief_registry_chr(related_ids),
    compliance = .brief_registry_normalize_compliance(compliance)
  )

  .brief_registry_validate_entry(entry)
}

#' Write a research-artifact plot registry to JSON
#'
#' @param plots List of registry entries, typically created with
#'   \code{brief_plot_registry_entry()}.
#' @param path Output JSON path.
#' @param schema_version Schema version string. Writers emit schema 2.0.
#' @param generated_at Generation timestamp. Defaults to current UTC time.
#' @param source_system Source-system label.
#' @param base_path_mode Path-mode label. Defaults to \code{"relative"}.
#' @param ready_only If \code{TRUE}, only keep entries with
#'   \code{status = "ready"}.
#' @param pretty If \code{TRUE}, write pretty JSON.
#' @param published_only Deprecated schema 1.0 alias for \code{ready_only}.
#'
#' @return Invisibly returns the registry object written to disk.
#' @export
brief_plot_registry_write <- function(
  plots,
  path = file.path("output", "publishing", "plot-registry.json"),
  schema_version = "2.0",
  generated_at = Sys.time(),
  source_system = "investlabr",
  base_path_mode = "relative",
  ready_only = FALSE,
  pretty = TRUE,
  published_only = NULL
) {
  if (!is.null(published_only)) {
    .brief_registry_warn_legacy("published_only")
    ready_only <- isTRUE(published_only)
  }
  if (!identical(as.character(schema_version), "2.0")) {
    stop("Registry writers emit only schema version 2.0.", call. = FALSE)
  }
  if (is.null(plots)) plots <- list()
  if (!is.list(plots)) stop("`plots` must be a list.", call. = FALSE)

  normalized_plots <- lapply(plots, .brief_registry_entry_from_list)
  if (isTRUE(ready_only)) {
    normalized_plots <- Filter(function(x) identical(x$status, "ready"), normalized_plots)
  }
  normalized_plots <- lapply(normalized_plots, .brief_registry_prepare_for_json)

  reg <- list(
    schema_version = "2.0",
    generated_at = .brief_registry_format_timestamp(generated_at),
    source_system = as.character(source_system),
    base_path_mode = as.character(base_path_mode),
    plots = normalized_plots
  )

  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  jsonlite::write_json(reg, path = path, auto_unbox = TRUE, pretty = pretty, null = "null")
  invisible(reg)
}

#' Build and write a research-artifact registry from YAML sidecars
#'
#' @param meta_dir Directory containing one-plot YAML sidecars.
#' @param path Output JSON path.
#' @param output_root Root directory against which relative asset paths are
#'   validated.
#' @inheritParams brief_plot_registry_write
#'
#' @return Invisibly returns the registry object written to disk.
#' @export
brief_plot_registry_write_from_meta <- function(
  meta_dir = file.path("config", "publishing", "plots"),
  path = file.path("output", "publishing", "plot-registry.json"),
  output_root = dirname(path),
  schema_version = "2.0",
  source_system = "investlabr",
  base_path_mode = "relative",
  ready_only = FALSE,
  pretty = TRUE,
  published_only = NULL
) {
  plots <- .brief_registry_load_meta_dir(meta_dir = meta_dir, output_root = output_root)
  brief_plot_registry_write(
    plots = plots,
    path = path,
    schema_version = schema_version,
    generated_at = Sys.time(),
    source_system = source_system,
    base_path_mode = base_path_mode,
    ready_only = ready_only,
    pretty = pretty,
    published_only = published_only
  )
}

#' Validate a research-artifact plot registry
#'
#' @param registry Registry object or path to a registry JSON file. Schema 1.0
#'   inputs are normalized for compatibility; schema 2.0 is current.
#' @param output_root Root directory against which relative asset paths are
#'   validated. Defaults to the registry file's directory when \code{registry}
#'   is a path.
#' @param require_assets Whether referenced plot, thumbnail, and optional HTML
#'   files must exist.
#'
#' @return A named validation summary containing plot counts and ids.
#' @export
brief_plot_registry_validate <- function(registry, output_root = NULL, require_assets = TRUE) {
  registry_path <- NULL
  if (is.character(registry) && length(registry) == 1L) {
    registry_path <- registry
    if (!file.exists(registry_path)) {
      stop("Registry file does not exist: ", registry_path, call. = FALSE)
    }
    registry <- jsonlite::read_json(registry_path, simplifyVector = FALSE)
  }
  if (!is.list(registry)) stop("`registry` must be a registry list or JSON path.", call. = FALSE)

  required_top <- c("schema_version", "generated_at", "source_system", "base_path_mode", "plots")
  missing_top <- setdiff(required_top, names(registry))
  if (length(missing_top) > 0L) {
    stop("Registry is missing top-level fields: ", paste(missing_top, collapse = ", "), call. = FALSE)
  }
  schema_version <- as.character(registry$schema_version)
  if (!schema_version %in% c("1.0", "2.0")) {
    stop("Registry `schema_version` must be `1.0` or `2.0`.", call. = FALSE)
  }
  if (!identical(as.character(registry$base_path_mode), "relative")) {
    stop("Registry `base_path_mode` must be `relative`.", call. = FALSE)
  }
  if (!is.list(registry$plots)) stop("Registry `plots` must be a list.", call. = FALSE)

  if (identical(schema_version, "2.0")) {
    entries <- lapply(seq_along(registry$plots), function(i) {
      entry <- registry$plots[[i]]
      .brief_registry_assert_v2_shape(entry, paste0("registry entry ", i))
      .brief_registry_entry_from_list(entry)
    })
  } else {
    entries <- lapply(registry$plots, .brief_registry_entry_from_list)
  }
  ids <- vapply(entries, `[[`, character(1), "id")
  if (anyDuplicated(ids)) stop("Registry plot ids must be unique.", call. = FALSE)

  if (isTRUE(require_assets)) {
    if (is.null(output_root)) {
      output_root <- if (is.null(registry_path)) "." else dirname(registry_path)
    }
    for (entry in entries) {
      .brief_registry_assert_asset_exists(output_root, entry$plot_image, "registry")
      .brief_registry_assert_asset_exists(output_root, entry$thumbnail, "registry")
      if (nzchar(entry$plot_html)) {
        .brief_registry_assert_asset_exists(output_root, entry$plot_html, "registry")
      }
    }
  }

  list(
    valid = TRUE,
    schema_version = schema_version,
    plot_count = length(entries),
    ready_count = sum(vapply(entries, function(x) identical(x$status, "ready"), logical(1))),
    ids = ids
  )
}

.brief_registry_load_meta_dir <- function(meta_dir, output_root) {
  if (!dir.exists(meta_dir)) return(list())
  meta_files <- sort(c(
    Sys.glob(file.path(meta_dir, "*.yml")),
    Sys.glob(file.path(meta_dir, "*.yaml"))
  ))
  if (length(meta_files) == 0L) return(list())

  lapply(meta_files, function(meta_file) {
    entry <- yaml::read_yaml(meta_file)
    if (!is.list(entry) || length(entry) == 0L) {
      stop("Metadata file did not produce a named list: ", meta_file, call. = FALSE)
    }
    .brief_registry_assert_v2_shape(entry, meta_file)
    entry <- .brief_registry_entry_from_list(entry)
    .brief_registry_assert_asset_exists(output_root, entry$plot_image, meta_file)
    .brief_registry_assert_asset_exists(output_root, entry$thumbnail, meta_file)
    if (nzchar(entry$plot_html)) {
      .brief_registry_assert_asset_exists(output_root, entry$plot_html, meta_file)
    }
    entry
  })
}

.brief_registry_entry_from_list <- function(entry) {
  is_v1 <- is.null(entry$collection) && !is.null(entry$dashboard)
  collection <- if (is_v1) entry$dashboard else entry$collection
  status <- entry$status
  if (is.null(status) && !is.null(entry$published)) {
    status <- if (isTRUE(entry$published)) "ready" else "draft"
  }
  priority <- entry$curation_priority
  if (is.null(priority)) priority <- .brief_registry_value_or_default(entry$sort_priority, 0L)

  brief_plot_registry_entry(
    id = entry$id,
    title = entry$title,
    subtitle = .brief_registry_value_or_default(entry$subtitle, ""),
    summary = .brief_registry_value_or_default(entry$summary, ""),
    description_md = .brief_registry_value_or_default(entry$description_md, ""),
    collection = collection,
    section = .brief_registry_value_or_default(entry$section, ""),
    asset_class = entry$asset_class,
    indicator_family = entry$indicator_family,
    region = entry$region,
    frequency = entry$frequency,
    source = entry$source,
    source_detail = .brief_registry_chr(entry$source_detail),
    tags = .brief_registry_chr(entry$tags),
    last_updated = entry$last_updated,
    status = status,
    curation_priority = priority,
    plot_image = entry$plot_image,
    thumbnail = entry$thumbnail,
    plot_html = .brief_registry_value_or_default(entry$plot_html, ""),
    related_ids = .brief_registry_chr(entry$related_ids),
    compliance = .brief_registry_value_or_default(
      entry$compliance,
      list(educational_only = TRUE, not_investment_advice = TRUE)
    )
  )
}

.brief_registry_assert_v2_shape <- function(entry, context) {
  if (!is.list(entry)) {
    stop("Schema 2.0 ", context, " must be a named list.", call. = FALSE)
  }
  legacy_fields <- c(
    "dashboard", "published", "featured", "sort_priority", "canonical_slug",
    "podcast_topics", "report_topics"
  )
  present_legacy <- intersect(names(entry), legacy_fields)
  if (length(present_legacy) > 0L) {
    stop(
      "Schema 2.0 ", context, " contains legacy consumer field(s): ",
      paste(present_legacy, collapse = ", "),
      call. = FALSE
    )
  }

  allowed_fields <- c(
    "id", "title", "subtitle", "summary", "description_md", "collection",
    "section", "asset_class", "indicator_family", "region", "frequency",
    "source", "source_detail", "tags", "last_updated", "status",
    "curation_priority", "plot_image", "thumbnail", "plot_html",
    "related_ids", "compliance"
  )
  unknown_fields <- setdiff(names(entry), allowed_fields)
  if (length(unknown_fields) > 0L) {
    stop(
      "Schema 2.0 ", context, " contains unknown field(s): ",
      paste(unknown_fields, collapse = ", "),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

.brief_registry_value_or_default <- function(x, default) {
  if (is.null(x)) default else x
}

.brief_registry_chr <- function(x) {
  if (is.null(x)) return(character())
  trimws(as.character(unlist(x, use.names = FALSE)))
}

.brief_registry_scalar_chr <- function(x) {
  out <- .brief_registry_chr(x)
  if (length(out) == 0L) return("")
  out[[1L]]
}

.brief_registry_format_date <- function(x) {
  if (inherits(x, "Date")) return(format(x, "%Y-%m-%d"))
  out <- as.character(x)
  if (length(out) != 1L || !grepl("^\\d{4}-\\d{2}-\\d{2}$", out)) {
    stop("`last_updated` must be an ISO date like YYYY-MM-DD.", call. = FALSE)
  }
  out
}

.brief_registry_format_timestamp <- function(x) {
  format(as.POSIXct(x, tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
}

.brief_registry_normalize_rel_path <- function(x, allow_empty = FALSE) {
  x <- as.character(x)
  if (length(x) != 1L) stop("Asset paths must be length-1 character strings.", call. = FALSE)
  if (!nzchar(x)) {
    if (allow_empty) return("")
    stop("Asset path must not be empty.", call. = FALSE)
  }
  x <- gsub("\\\\", "/", x)
  if (.brief_registry_is_absolute_path(x)) {
    stop("Asset paths must be relative, not absolute: ", x, call. = FALSE)
  }
  if (any(strsplit(x, "/", fixed = TRUE)[[1L]] == "..")) {
    stop("Asset paths must not traverse outside the output root: ", x, call. = FALSE)
  }
  x
}

.brief_registry_is_absolute_path <- function(x) {
  grepl("^(/|[A-Za-z]:[/\\\\])", x)
}

.brief_registry_normalize_compliance <- function(x) {
  if (is.null(x)) {
    x <- list(educational_only = TRUE, not_investment_advice = TRUE)
  }
  if (!is.list(x)) stop("`compliance` must be a list.", call. = FALSE)
  list(
    educational_only = isTRUE(x$educational_only),
    not_investment_advice = isTRUE(x$not_investment_advice)
  )
}

.brief_registry_prepare_for_json <- function(entry) {
  array_fields <- c("source_detail", "tags", "related_ids")
  for (nm in array_fields) entry[[nm]] <- I(as.list(unname(entry[[nm]])))
  entry
}

.brief_registry_validate_entry <- function(entry) {
  required_fields <- c(
    "id", "title", "collection", "asset_class", "indicator_family", "region",
    "frequency", "source", "last_updated", "status", "plot_image",
    "thumbnail", "tags"
  )
  missing_fields <- required_fields[!required_fields %in% names(entry)]
  if (length(missing_fields) > 0L) {
    stop("Missing required registry fields: ", paste(missing_fields, collapse = ", "), call. = FALSE)
  }

  scalar_fields <- c(
    "id", "title", "collection", "asset_class", "indicator_family", "region",
    "frequency", "source", "status", "plot_image", "thumbnail"
  )
  for (nm in scalar_fields) {
    if (!is.character(entry[[nm]]) || length(entry[[nm]]) != 1L || !nzchar(entry[[nm]])) {
      stop("`", nm, "` must be a non-empty length-1 character value.", call. = FALSE)
    }
  }
  if (!grepl("^[a-z0-9]+(?:-[a-z0-9]+)*$", entry$id)) {
    stop("`id` must be lowercase kebab-case.", call. = FALSE)
  }

  allowed_collections <- c("macro", "markets", "crypto", "strategies")
  if (!entry$collection %in% allowed_collections) {
    stop("`collection` must be one of: ", paste(allowed_collections, collapse = ", "), call. = FALSE)
  }
  allowed_status <- c("draft", "ready", "archived")
  if (!entry$status %in% allowed_status) {
    stop("`status` must be one of: ", paste(allowed_status, collapse = ", "), call. = FALSE)
  }
  allowed_frequency <- c("Daily", "Weekly", "Monthly", "Quarterly", "Event")
  if (!entry$frequency %in% allowed_frequency) {
    stop("`frequency` must be one of: ", paste(allowed_frequency, collapse = ", "), call. = FALSE)
  }
  allowed_asset_class <- c(
    "Rates", "Liquidity", "Inflation", "Equities", "Bonds", "FX",
    "Commodities", "Crypto", "Strategy Signals"
  )
  if (!entry$asset_class %in% allowed_asset_class) {
    stop("`asset_class` must be one of: ", paste(allowed_asset_class, collapse = ", "), call. = FALSE)
  }
  allowed_region <- c(
    "United States", "Europe", "Japan", "China", "Global", "Emerging Markets"
  )
  if (!entry$region %in% allowed_region) {
    stop("`region` must be one of: ", paste(allowed_region, collapse = ", "), call. = FALSE)
  }

  entry$last_updated <- .brief_registry_format_date(entry$last_updated)
  entry$plot_image <- .brief_registry_normalize_rel_path(entry$plot_image)
  entry$thumbnail <- .brief_registry_normalize_rel_path(entry$thumbnail)
  entry$plot_html <- .brief_registry_normalize_rel_path(entry$plot_html, allow_empty = TRUE)
  entry$source_detail <- .brief_registry_chr(entry$source_detail)
  entry$tags <- .brief_registry_chr(entry$tags)
  entry$related_ids <- .brief_registry_chr(entry$related_ids)
  entry$curation_priority <- as.integer(entry$curation_priority)
  entry$compliance <- .brief_registry_normalize_compliance(entry$compliance)

  if (length(entry$tags) == 0L || any(!nzchar(entry$tags))) {
    stop("`tags` must be a non-empty flat character vector.", call. = FALSE)
  }
  if (length(entry$curation_priority) != 1L || is.na(entry$curation_priority)) {
    stop("`curation_priority` must be one finite integer.", call. = FALSE)
  }
  entry
}

.brief_registry_assert_asset_exists <- function(output_root, rel_path, meta_file) {
  asset_path <- file.path(output_root, rel_path)
  if (!file.exists(asset_path)) {
    stop(
      "Registry asset path does not exist for metadata file ",
      meta_file, ": ", rel_path,
      call. = FALSE
    )
  }
}

.brief_registry_warn_legacy <- function(fields) {
  warning(
    "Schema 1.0 registry field(s) are deprecated: ",
    paste(fields, collapse = ", "),
    ". Use schema 2.0 consumer-neutral fields instead.",
    call. = FALSE
  )
}
