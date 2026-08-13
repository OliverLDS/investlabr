#' Calculate a conservative artifact data date
#'
#' Calculates the minimum of the latest usable observation date for every
#' required input series. This prevents a high-frequency series from making a
#' mixed-frequency artifact appear fresher than its stalest required input.
#'
#' @param series Named list of date vectors or data frames containing a date
#'   column. Inputs should already exclude observations that were not actually
#'   consumed by the artifact.
#' @param date_col Date-column name used for data-frame inputs.
#' @param completed_before Optional exclusive upper date bound. For Yahoo data,
#'   pass the current UTC date to exclude a potentially incomplete current-day
#'   bar.
#'
#' @return An ISO date string representing the latest common usable
#'   observation.
#' @export
brief_data_as_of <- function(series, date_col = "date", completed_before = NULL) {
  if (!is.list(series) || length(series) == 0L) {
    stop("`series` must be a non-empty named list.", call. = FALSE)
  }
  if (is.null(names(series)) || any(!nzchar(names(series)))) {
    stop("`series` must have non-empty names.", call. = FALSE)
  }
  if (!is.null(completed_before)) {
    completed_before <- .brief_registry_format_date(completed_before, "completed_before")
    completed_before <- as.Date(completed_before)
  }

  latest <- vapply(names(series), function(nm) {
    x <- series[[nm]]
    dates <- if (is.data.frame(x)) {
      if (!date_col %in% names(x)) {
        stop("Series `", nm, "` has no `", date_col, "` column.", call. = FALSE)
      }
      x[[date_col]]
    } else {
      x
    }
    dates <- as.Date(dates)
    dates <- dates[!is.na(dates)]
    if (!is.null(completed_before)) dates <- dates[dates < completed_before]
    if (length(dates) == 0L) {
      stop("Series `", nm, "` has no usable observation date.", call. = FALSE)
    }
    format(max(dates), "%Y-%m-%d")
  }, character(1))

  format(min(as.Date(latest)), "%Y-%m-%d")
}

#' Build a deprecated schema 2.0 research-artifact registry entry
#'
#' This constructor is retained for source compatibility. New code should use
#' \code{brief_plot_registry_entry_v3()}. Legacy dates are never interpreted as
#' artifact render times or editorial metadata dates.
#'
#' @param id,title,asset_class,indicator_family,region,frequency,source,plot_image,thumbnail,tags
#'   Core registry fields.
#' @param last_updated Legacy schema 2.0 artifact date.
#' @param collection,status Nullable schema 2.0 fields. The deprecated
#'   \code{dashboard} and \code{published} arguments are used as fallbacks.
#' @param subtitle,summary,description_md,section,source_detail,curation_priority,plot_html,related_ids,compliance
#'   Optional schema 2.0 fields.
#' @param dashboard,published,featured,sort_priority,canonical_slug,podcast_topics,report_topics
#'   Deprecated schema 1.0 compatibility fields.
#'
#' @return A validated schema 2.0 entry that cannot be emitted as schema 3.0.
#' @export
brief_plot_registry_entry <- function(
  id, title, collection = NULL, asset_class, indicator_family, region,
  frequency, source, last_updated, status = NULL, plot_image, thumbnail, tags,
  subtitle = "", summary = "", description_md = "", section = "",
  source_detail = character(), curation_priority = 0, plot_html = "",
  related_ids = character(), compliance = list(
    educational_only = TRUE, not_investment_advice = TRUE
  ),
  dashboard = NULL, published = NULL, featured = NULL, sort_priority = NULL,
  canonical_slug = NULL, podcast_topics = NULL, report_topics = NULL
) {
  warning(
    "`brief_plot_registry_entry()` is deprecated for new construction; use `brief_plot_registry_entry_v3()` for schema 3.0 metadata.",
    call. = FALSE
  )
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
    last_updated = .brief_registry_format_date(last_updated, "last_updated"),
    status = .brief_registry_scalar_chr(status),
    curation_priority = as.integer(curation_priority),
    plot_image = .brief_registry_normalize_rel_path(plot_image),
    thumbnail = .brief_registry_normalize_rel_path(thumbnail),
    plot_html = .brief_registry_normalize_rel_path(plot_html, allow_empty = TRUE),
    related_ids = .brief_registry_chr(related_ids),
    compliance = .brief_registry_normalize_compliance(compliance)
  )
  .brief_registry_validate_v2_entry(entry)
}

#' Build one canonical schema 3.0 research-artifact registry entry
#'
#' @param id Stable machine-safe plot identifier in lowercase kebab-case.
#' @param title Reader-facing plot title.
#' @param collection High-level research collection. One of \code{"macro"},
#'   \code{"markets"}, \code{"crypto"}, or \code{"strategies"}.
#' @param asset_class Asset-class label such as \code{"Rates"}.
#' @param indicator_family Indicator-family label.
#' @param region Region label.
#' @param frequency Frequency label.
#' @param source Source label.
#' @param rendered_at UTC artifact-render timestamp in
#'   \code{YYYY-MM-DDTHH:MM:SSZ} format. Required for ready artifacts.
#' @param data_as_of Latest observation actually represented by the artifact,
#'   as an ISO date. Required for ready time-indexed artifacts.
#' @param metadata_updated_at ISO date of the latest material human-authored
#'   metadata change.
#' @param time_indexed Whether the artifact contains time-indexed data.
#' @param expected_cadence Expected update cadence of code{data_as_of}. One of
#'   code{"daily"}, code{"weekly"}, code{"monthly"},
#'   code{"event_driven"}, or code{"not_time_indexed"}.
#' @param status Artifact readiness: \code{"draft"}, \code{"ready"}, or
#'   \code{"archived"}.
#' @param plot_image Relative primary-plot path.
#' @param thumbnail Relative thumbnail path.
#' @param tags Flat character vector of tags.
#' @param subtitle,summary,description_md,section Optional descriptive fields.
#' @param source_detail Optional source details.
#' @param curation_priority Integer curation priority.
#' @param plot_html Optional relative HTML path.
#' @param related_ids Related artifact ids.
#' @param compliance Named compliance flags.
#'
#' @return A canonical schema 3.0 entry. Writers may project it to schema 2.0.
#' @export
brief_plot_registry_entry_v3 <- function(
  id, title, collection, asset_class, indicator_family, region, frequency,
  source, rendered_at = NULL, data_as_of = NULL, metadata_updated_at,
  time_indexed = TRUE, expected_cadence, status, plot_image, thumbnail, tags,
  subtitle = "", summary = "", description_md = "", section = "",
  source_detail = character(), curation_priority = 0, plot_html = "",
  related_ids = character(), compliance = list(
    educational_only = TRUE, not_investment_advice = TRUE
  )
) {
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
    rendered_at = .brief_registry_nullable_timestamp(rendered_at, "rendered_at"),
    data_as_of = .brief_registry_nullable_date(data_as_of, "data_as_of"),
    metadata_updated_at = .brief_registry_format_date(metadata_updated_at, "metadata_updated_at"),
    time_indexed = .brief_registry_scalar_logical(time_indexed, "time_indexed"),
    expected_cadence = .brief_registry_scalar_chr(expected_cadence),
    status = .brief_registry_scalar_chr(status),
    curation_priority = as.integer(curation_priority),
    plot_image = .brief_registry_normalize_rel_path(plot_image),
    thumbnail = .brief_registry_normalize_rel_path(thumbnail),
    plot_html = .brief_registry_normalize_rel_path(plot_html, allow_empty = TRUE),
    related_ids = .brief_registry_chr(related_ids),
    compliance = .brief_registry_normalize_compliance(compliance)
  )
  .brief_registry_validate_v3_entry(entry)
}

#' Write a research-artifact plot registry to JSON
#'
#' @param plots Schema 2.0 entries from \code{brief_plot_registry_entry()} or
#'   canonical schema 3.0 entries from \code{brief_plot_registry_entry_v3()}.
#' @param path Output JSON path.
#' @param schema_version Output schema. Defaults to compatibility schema 2.0;
#'   use \code{"3.0"} explicitly for migration testing.
#' @param generated_at Registry-generation timestamp in UTC.
#' @param source_system Source-system label.
#' @param base_path_mode Must be \code{"relative"}.
#' @param ready_only Keep only ready entries.
#' @param pretty Write pretty JSON.
#' @param published_only Deprecated alias for \code{ready_only}.
#'
#' @return Invisibly returns the registry written to disk.
#' @export
brief_plot_registry_write <- function(
  plots, path = file.path("output", "publishing", "plot-registry.json"),
  schema_version = "2.0", generated_at = Sys.time(),
  source_system = "investlabr", base_path_mode = "relative",
  ready_only = FALSE, pretty = TRUE, published_only = NULL
) {
  if (!is.null(published_only)) {
    warning("`published_only` is deprecated; use `ready_only`.", call. = FALSE)
    ready_only <- isTRUE(published_only)
  }
  schema_version <- as.character(schema_version)
  if (!schema_version %in% c("2.0", "3.0")) {
    stop("Registry writers emit schema version 2.0 or 3.0.", call. = FALSE)
  }
  if (is.null(plots)) plots <- list()
  if (!is.list(plots)) stop("`plots` must be a list.", call. = FALSE)
  emitted <- if (identical(schema_version, "2.0")) {
    lapply(plots, function(entry) {
      if (.brief_registry_is_v3_entry(entry)) {
        .brief_registry_v3_to_v2(.brief_registry_v3_from_list(entry))
      } else {
        .brief_registry_assert_v2_shape(entry, "writer entry")
        .brief_registry_validate_v2_entry(entry)
      }
    })
  } else {
    lapply(plots, function(entry) {
      if (!.brief_registry_is_v3_entry(entry)) {
        stop(
          "Schema 2.0 entries cannot be emitted as schema 3.0 without explicit `rendered_at`, `data_as_of`, and `metadata_updated_at`; reconstruct with `brief_plot_registry_entry_v3()`.",
          call. = FALSE
        )
      }
      .brief_registry_v3_from_list(entry)
    })
  }
  if (isTRUE(ready_only)) {
    emitted <- Filter(function(x) identical(x$status, "ready"), emitted)
  }
  emitted <- lapply(emitted, .brief_registry_prepare_for_json)
  generated_at <- .brief_registry_format_timestamp(generated_at, "generated_at")

  reg <- list(
    schema_version = schema_version,
    generated_at = generated_at,
    source_system = .brief_registry_scalar_chr(source_system),
    base_path_mode = .brief_registry_scalar_chr(base_path_mode),
    plots = emitted
  )
  brief_plot_registry_validate(reg, require_assets = FALSE)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  jsonlite::write_json(reg, path = path, auto_unbox = TRUE, pretty = pretty, null = "null")
  invisible(reg)
}

#' Build and write a research-artifact registry from metadata sidecars
#'
#' @param meta_dir Tracked YAML sidecar directory.
#' @param path Output JSON path.
#' @param output_root Root for relative asset paths.
#' @param resolved_meta_dir Run-local metadata emitted by the renderer.
#' @inheritParams brief_plot_registry_write
#'
#' @return Invisibly returns the registry written to disk.
#' @export
brief_plot_registry_write_from_meta <- function(
  meta_dir = file.path("config", "publishing", "plots"),
  path = file.path("output", "publishing", "plot-registry.json"),
  output_root = dirname(path),
  resolved_meta_dir = file.path(output_root, "resolved"),
  schema_version = "2.0", source_system = "investlabr",
  base_path_mode = "relative", ready_only = FALSE, pretty = TRUE,
  published_only = NULL
) {
  plots <- .brief_registry_load_meta_dir(meta_dir, output_root, resolved_meta_dir)
  brief_plot_registry_write(
    plots = plots, path = path, schema_version = schema_version,
    generated_at = Sys.time(), source_system = source_system,
    base_path_mode = base_path_mode, ready_only = ready_only, pretty = pretty,
    published_only = published_only
  )
}

#' Validate a research-artifact plot registry
#'
#' @param registry Registry object or JSON path. Schemas 1.0, 2.0, and 3.0 are
#'   readable; schema 2.0 remains the default writer output during migration.
#' @param output_root Root for relative asset validation.
#' @param require_assets Require referenced assets to exist.
#'
#' @return Named validation summary.
#' @export
brief_plot_registry_validate <- function(registry, output_root = NULL, require_assets = TRUE) {
  registry_path <- NULL
  if (is.character(registry) && length(registry) == 1L) {
    registry_path <- registry
    if (!file.exists(registry_path)) stop("Registry file does not exist: ", registry_path, call. = FALSE)
    registry <- jsonlite::read_json(registry_path, simplifyVector = FALSE)
  }
  if (!is.list(registry)) stop("`registry` must be a registry list or JSON path.", call. = FALSE)
  required_top <- c("schema_version", "generated_at", "source_system", "base_path_mode", "plots")
  missing_top <- setdiff(required_top, names(registry))
  if (length(missing_top)) stop("Registry is missing top-level fields: ", paste(missing_top, collapse = ", "), call. = FALSE)
  unknown_top <- setdiff(names(registry), required_top)
  if (length(unknown_top)) stop("Registry contains unknown top-level fields: ", paste(unknown_top, collapse = ", "), call. = FALSE)

  schema <- as.character(registry$schema_version)
  if (!schema %in% c("1.0", "2.0", "3.0")) stop("Registry `schema_version` must be `1.0`, `2.0`, or `3.0`.", call. = FALSE)
  generated_at <- .brief_registry_format_timestamp(registry$generated_at, "generated_at")
  if (!identical(as.character(registry$base_path_mode), "relative")) stop("Registry `base_path_mode` must be `relative`.", call. = FALSE)
  if (!is.list(registry$plots)) stop("Registry `plots` must be a list.", call. = FALSE)

  entries <- lapply(seq_along(registry$plots), function(i) {
    entry <- registry$plots[[i]]
    context <- paste0("registry entry ", i)
    if (identical(schema, "3.0")) {
      .brief_registry_assert_v3_shape(entry, context)
      entry <- .brief_registry_v3_from_list(entry)
      if (identical(entry$status, "ready") && .brief_registry_parse_timestamp(entry$rendered_at) > .brief_registry_parse_timestamp(generated_at)) {
        stop("Registry `generated_at` must be equal to or later than every ready artifact's `rendered_at`.", call. = FALSE)
      }
      entry
    } else if (identical(schema, "2.0")) {
      .brief_registry_assert_v2_shape(entry, context)
      .brief_registry_validate_v2_entry(entry)
    } else {
      .brief_registry_validate_v1_entry(entry)
    }
  })
  ids <- vapply(entries, `[[`, character(1), "id")
  if (anyDuplicated(ids)) stop("Registry plot ids must be unique.", call. = FALSE)

  if (isTRUE(require_assets)) {
    if (is.null(output_root)) output_root <- if (is.null(registry_path)) "." else dirname(registry_path)
    for (entry in entries) {
      .brief_registry_assert_asset_exists(output_root, entry$plot_image, "registry")
      .brief_registry_assert_asset_exists(output_root, entry$thumbnail, "registry")
      if (nzchar(.brief_registry_value_or_default(entry$plot_html, ""))) {
        .brief_registry_assert_asset_exists(output_root, entry$plot_html, "registry")
      }
    }
  }
  list(
    valid = TRUE, schema_version = schema, plot_count = length(entries),
    ready_count = sum(vapply(entries, function(x) identical(x$status, "ready"), logical(1))),
    ids = ids
  )
}

.brief_registry_load_meta_dir <- function(meta_dir, output_root, resolved_meta_dir) {
  if (!dir.exists(meta_dir)) return(list())
  files <- sort(c(Sys.glob(file.path(meta_dir, "*.yml")), Sys.glob(file.path(meta_dir, "*.yaml"))))
  lapply(files, function(path) {
    tracked <- yaml::read_yaml(path)
    .brief_registry_assert_sidecar_shape(tracked, path)
    resolved_path <- file.path(resolved_meta_dir, paste0(tracked$id, ".json"))
    resolved <- if (file.exists(resolved_path)) jsonlite::read_json(resolved_path, simplifyVector = FALSE) else list()
    if (identical(tracked$status, "ready") && length(resolved) == 0L) {
      stop("Ready artifact has no run-local resolved metadata; render it first: ", tracked$id, call. = FALSE)
    }
    if (length(resolved)) {
      allowed <- c("id", "rendered_at", "data_as_of", "metadata_updated_at", "time_indexed", "expected_cadence", "data_as_of_rule")
      unknown <- setdiff(names(resolved), allowed)
      if (length(unknown)) stop("Resolved metadata contains unknown fields: ", paste(unknown, collapse = ", "), call. = FALSE)
      if (!identical(as.character(resolved$id), as.character(tracked$id))) stop("Resolved metadata id mismatch for ", tracked$id, call. = FALSE)
      if (!identical(as.character(resolved$metadata_updated_at), as.character(tracked$metadata_updated_at))) {
        stop("Resolved metadata is stale relative to tracked metadata for ", tracked$id, call. = FALSE)
      }
      if (!identical(isTRUE(resolved$time_indexed), isTRUE(tracked$time_indexed))) {
        stop("Resolved metadata `time_indexed` is stale relative to tracked metadata for ", tracked$id, call. = FALSE)
      }
      if (!identical(as.character(resolved$expected_cadence), as.character(tracked$expected_cadence))) {
        stop("Resolved metadata `expected_cadence` is stale relative to tracked metadata for ", tracked$id, call. = FALSE)
      }
    }
    entry <- .brief_registry_v3_from_list(c(
      tracked,
      list(
        rendered_at = resolved$rendered_at,
        data_as_of = resolved$data_as_of
      )
    ))
    .brief_registry_assert_asset_exists(output_root, entry$plot_image, path)
    .brief_registry_assert_asset_exists(output_root, entry$thumbnail, path)
    if (nzchar(entry$plot_html)) .brief_registry_assert_asset_exists(output_root, entry$plot_html, path)
    entry
  })
}

.brief_registry_v3_from_list <- function(x) {
  brief_plot_registry_entry_v3(
    id = x$id, title = x$title, collection = x$collection,
    asset_class = x$asset_class, indicator_family = x$indicator_family,
    region = x$region, frequency = x$frequency, source = x$source,
    rendered_at = x$rendered_at, data_as_of = x$data_as_of,
    metadata_updated_at = x$metadata_updated_at,
    time_indexed = .brief_registry_value_or_default(x$time_indexed, TRUE),
    expected_cadence = x$expected_cadence,
    status = x$status, plot_image = x$plot_image, thumbnail = x$thumbnail,
    tags = x$tags, subtitle = .brief_registry_value_or_default(x$subtitle, ""),
    summary = .brief_registry_value_or_default(x$summary, ""),
    description_md = .brief_registry_value_or_default(x$description_md, ""),
    section = .brief_registry_value_or_default(x$section, ""),
    source_detail = x$source_detail,
    curation_priority = .brief_registry_value_or_default(x$curation_priority, 0L),
    plot_html = .brief_registry_value_or_default(x$plot_html, ""),
    related_ids = x$related_ids,
    compliance = .brief_registry_value_or_default(x$compliance, list(educational_only = TRUE, not_investment_advice = TRUE))
  )
}

.brief_registry_is_v3_entry <- function(x) {
  is.list(x) && all(c(
    "rendered_at", "data_as_of", "metadata_updated_at", "time_indexed", "expected_cadence"
  ) %in% names(x)) && !"last_updated" %in% names(x)
}

.brief_registry_v3_to_v2 <- function(x) {
  out <- x
  out$last_updated <- x$data_as_of
  out[c("rendered_at", "data_as_of", "metadata_updated_at", "time_indexed", "expected_cadence")] <- NULL
  order <- c(
    "id", "title", "subtitle", "summary", "description_md", "collection",
    "section", "asset_class", "indicator_family", "region", "frequency",
    "source", "source_detail", "tags", "last_updated", "status",
    "curation_priority", "plot_image", "thumbnail", "plot_html",
    "related_ids", "compliance"
  )
  out[order]
}

.brief_registry_assert_sidecar_shape <- function(x, context) {
  if (!is.list(x)) stop("Metadata sidecar must be a named list: ", context, call. = FALSE)
  forbidden <- intersect(names(x), c("last_updated", "rendered_at", "data_as_of"))
  if (length(forbidden)) stop("Tracked metadata must not contain runtime/legacy field(s): ", paste(forbidden, collapse = ", "), call. = FALSE)
  allowed <- setdiff(.brief_registry_v3_fields(), c("rendered_at", "data_as_of"))
  unknown <- setdiff(names(x), allowed)
  if (length(unknown)) stop("Metadata sidecar contains unknown field(s): ", paste(unknown, collapse = ", "), call. = FALSE)
  required <- c("id", "title", "collection", "asset_class", "indicator_family", "region", "frequency", "source", "metadata_updated_at", "time_indexed", "expected_cadence", "status", "plot_image", "thumbnail", "tags")
  missing <- setdiff(required, names(x))
  if (length(missing)) stop("Metadata sidecar is missing fields: ", paste(missing, collapse = ", "), call. = FALSE)
  .brief_registry_format_date(x$metadata_updated_at, "metadata_updated_at")
  invisible(TRUE)
}

.brief_registry_assert_v3_shape <- function(x, context) {
  if (!is.list(x)) stop("Schema 3.0 ", context, " must be a named list.", call. = FALSE)
  if ("last_updated" %in% names(x)) stop("Schema 3.0 forbids `last_updated`.", call. = FALSE)
  unknown <- setdiff(names(x), .brief_registry_v3_fields())
  if (length(unknown)) stop("Schema 3.0 ", context, " contains unknown field(s): ", paste(unknown, collapse = ", "), call. = FALSE)
  missing <- setdiff(.brief_registry_v3_fields(), names(x))
  if (length(missing)) stop("Schema 3.0 ", context, " is missing field(s): ", paste(missing, collapse = ", "), call. = FALSE)
  invisible(TRUE)
}

.brief_registry_v3_fields <- function() c(
  "id", "title", "subtitle", "summary", "description_md", "collection",
  "section", "asset_class", "indicator_family", "region", "frequency",
  "source", "source_detail", "tags", "rendered_at", "data_as_of",
  "metadata_updated_at", "time_indexed", "expected_cadence", "status", "curation_priority",
  "plot_image", "thumbnail", "plot_html", "related_ids", "compliance"
)

.brief_registry_assert_v2_shape <- function(x, context) {
  if (!is.list(x)) stop("Schema 2.0 ", context, " must be a named list.", call. = FALSE)
  legacy <- intersect(names(x), c("dashboard", "published", "featured", "sort_priority", "canonical_slug", "podcast_topics", "report_topics"))
  if (length(legacy)) stop("Schema 2.0 ", context, " contains legacy consumer field(s): ", paste(legacy, collapse = ", "), call. = FALSE)
  allowed <- c("id", "title", "subtitle", "summary", "description_md", "collection", "section", "asset_class", "indicator_family", "region", "frequency", "source", "source_detail", "tags", "last_updated", "status", "curation_priority", "plot_image", "thumbnail", "plot_html", "related_ids", "compliance")
  unknown <- setdiff(names(x), allowed)
  if (length(unknown)) stop("Schema 2.0 ", context, " contains unknown field(s): ", paste(unknown, collapse = ", "), call. = FALSE)
  invisible(TRUE)
}

.brief_registry_validate_v3_entry <- function(x) {
  .brief_registry_validate_common(x)
  .brief_registry_assert_choice(
    x$expected_cadence,
    c("daily", "weekly", "monthly", "event_driven", "not_time_indexed"),
    "expected_cadence"
  )
  if (identical(x$expected_cadence, "not_time_indexed")) {
    if (isTRUE(x$time_indexed) || !is.null(x$data_as_of)) {
      stop("`not_time_indexed` requires `time_indexed: false` and `data_as_of: null`.", call. = FALSE)
    }
  } else if (!isTRUE(x$time_indexed)) {
    stop("Time-indexed expected cadences require `time_indexed: true`.", call. = FALSE)
  }
  if (identical(x$status, "ready") && is.null(x$rendered_at)) stop("Ready artifacts require non-null `rendered_at`.", call. = FALSE)
  if (identical(x$status, "ready") && isTRUE(x$time_indexed) && is.null(x$data_as_of)) stop("Ready time-indexed artifacts require non-null `data_as_of`.", call. = FALSE)
  if (!is.null(x$rendered_at)) {
    render_date <- as.Date(substr(x$rendered_at, 1L, 10L))
    if (!is.null(x$data_as_of) && as.Date(x$data_as_of) > render_date) stop("`data_as_of` cannot be later than the UTC date of `rendered_at`.", call. = FALSE)
    if (as.Date(x$metadata_updated_at) > render_date) stop("`metadata_updated_at` cannot be later than the UTC date of `rendered_at`.", call. = FALSE)
  }
  x
}

.brief_registry_validate_v2_entry <- function(x) {
  required <- c("id", "title", "collection", "asset_class", "indicator_family", "region", "frequency", "source", "last_updated", "status", "plot_image", "thumbnail", "tags")
  missing <- setdiff(required, names(x))
  if (length(missing)) stop("Schema 2.0 entry is missing fields: ", paste(missing, collapse = ", "), call. = FALSE)
  x$last_updated <- .brief_registry_format_date(x$last_updated, "last_updated")
  x <- .brief_registry_fill_common_defaults(x)
  .brief_registry_validate_common(x)
}

.brief_registry_validate_v1_entry <- function(x) {
  if (!is.list(x)) stop("Schema 1.0 entry must be a named list.", call. = FALSE)
  x$collection <- .brief_registry_value_or_default(x$collection, x$dashboard)
  x$status <- .brief_registry_value_or_default(x$status, if (isTRUE(x$published)) "ready" else "draft")
  x$curation_priority <- .brief_registry_value_or_default(x$curation_priority, .brief_registry_value_or_default(x$sort_priority, 0L))
  x <- .brief_registry_fill_common_defaults(x)
  x$last_updated <- .brief_registry_format_date(x$last_updated, "last_updated")
  .brief_registry_validate_common(x)
}

.brief_registry_fill_common_defaults <- function(x) {
  for (nm in c("subtitle", "summary", "description_md", "section", "plot_html")) x[[nm]] <- .brief_registry_value_or_default(x[[nm]], "")
  for (nm in c("source_detail", "related_ids")) x[[nm]] <- .brief_registry_chr(x[[nm]])
  x$curation_priority <- as.integer(.brief_registry_value_or_default(x$curation_priority, 0L))
  x$compliance <- .brief_registry_normalize_compliance(x$compliance)
  x
}

.brief_registry_validate_common <- function(x) {
  required <- c("id", "title", "collection", "asset_class", "indicator_family", "region", "frequency", "source", "status", "plot_image", "thumbnail", "tags")
  missing <- setdiff(required, names(x))
  if (length(missing)) stop("Missing required registry fields: ", paste(missing, collapse = ", "), call. = FALSE)
  for (nm in required[required != "tags"]) {
    if (!is.character(x[[nm]]) || length(x[[nm]]) != 1L || !nzchar(x[[nm]])) stop("`", nm, "` must be a non-empty length-1 character value.", call. = FALSE)
  }
  if (!grepl("^[a-z0-9]+(?:-[a-z0-9]+)*$", x$id)) stop("`id` must be lowercase kebab-case.", call. = FALSE)
  .brief_registry_assert_choice(x$collection, c("macro", "markets", "crypto", "strategies"), "collection")
  .brief_registry_assert_choice(x$status, c("draft", "ready", "archived"), "status")
  .brief_registry_assert_choice(x$frequency, c("Daily", "Weekly", "Monthly", "Quarterly", "Event"), "frequency")
  .brief_registry_assert_choice(x$asset_class, c("Rates", "Liquidity", "Inflation", "Equities", "Bonds", "FX", "Commodities", "Crypto", "Strategy Signals"), "asset_class")
  .brief_registry_assert_choice(x$region, c("United States", "Europe", "Japan", "China", "Global", "Emerging Markets"), "region")
  x$plot_image <- .brief_registry_normalize_rel_path(x$plot_image)
  x$thumbnail <- .brief_registry_normalize_rel_path(x$thumbnail)
  x$plot_html <- .brief_registry_normalize_rel_path(.brief_registry_value_or_default(x$plot_html, ""), allow_empty = TRUE)
  x$tags <- .brief_registry_chr(x$tags)
  if (!length(x$tags) || any(!nzchar(x$tags))) stop("`tags` must be a non-empty flat character vector.", call. = FALSE)
  x
}

.brief_registry_assert_choice <- function(x, allowed, name) if (!x %in% allowed) stop("`", name, "` must be one of: ", paste(allowed, collapse = ", "), call. = FALSE)
.brief_registry_value_or_default <- function(x, default) if (is.null(x)) default else x
.brief_registry_chr <- function(x) if (is.null(x)) character() else trimws(as.character(unlist(x, use.names = FALSE)))
.brief_registry_scalar_chr <- function(x) { out <- .brief_registry_chr(x); if (!length(out)) "" else out[[1L]] }
.brief_registry_scalar_logical <- function(x, name) { if (!is.logical(x) || length(x) != 1L || is.na(x)) stop("`", name, "` must be TRUE or FALSE.", call. = FALSE); x }

.brief_registry_format_date <- function(x, name = "date") {
  if (inherits(x, "Date")) x <- format(x, "%Y-%m-%d")
  x <- as.character(x)
  valid <- length(x) == 1L && grepl("^\\d{4}-\\d{2}-\\d{2}$", x)
  parsed <- if (valid) tryCatch(as.Date(x), error = function(e) as.Date(NA)) else as.Date(NA)
  if (!valid || is.na(parsed) || !identical(format(parsed, "%Y-%m-%d"), x)) stop("`", name, "` must be a valid ISO date like YYYY-MM-DD.", call. = FALSE)
  x
}

.brief_registry_nullable_date <- function(x, name) if (is.null(x) || (length(x) == 1L && is.na(x))) NULL else .brief_registry_format_date(x, name)

.brief_registry_format_timestamp <- function(x, name = "timestamp") {
  if (inherits(x, "POSIXt")) return(format(x, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"))
  x <- as.character(x)
  valid <- length(x) == 1L && grepl("^\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}Z$", x)
  parsed <- if (valid) .brief_registry_parse_timestamp(x) else as.POSIXct(NA)
  if (!valid || is.na(parsed) || !identical(format(parsed, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"), x)) stop("`", name, "` must be a UTC timestamp like YYYY-MM-DDTHH:MM:SSZ.", call. = FALSE)
  x
}

.brief_registry_nullable_timestamp <- function(x, name) if (is.null(x) || (length(x) == 1L && is.na(x))) NULL else .brief_registry_format_timestamp(x, name)

.brief_registry_parse_timestamp <- function(x) {
  as.POSIXct(x, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
}

.brief_registry_normalize_rel_path <- function(x, allow_empty = FALSE) {
  x <- as.character(x)
  if (length(x) != 1L) stop("Asset paths must be length-1 character strings.", call. = FALSE)
  if (!nzchar(x)) { if (allow_empty) return(""); stop("Asset path must not be empty.", call. = FALSE) }
  x <- gsub("\\\\", "/", x)
  if (grepl("^(/|[A-Za-z]:[/\\\\])", x)) stop("Asset paths must be relative, not absolute: ", x, call. = FALSE)
  if (any(strsplit(x, "/", fixed = TRUE)[[1L]] == "..")) stop("Asset paths must not traverse outside the output root: ", x, call. = FALSE)
  x
}

.brief_registry_normalize_compliance <- function(x) {
  if (is.null(x)) x <- list(educational_only = TRUE, not_investment_advice = TRUE)
  if (!is.list(x)) stop("`compliance` must be a list.", call. = FALSE)
  list(educational_only = isTRUE(x$educational_only), not_investment_advice = isTRUE(x$not_investment_advice))
}

.brief_registry_prepare_for_json <- function(x) {
  for (nm in c("source_detail", "tags", "related_ids")) x[[nm]] <- I(as.list(unname(x[[nm]])))
  x
}

.brief_registry_assert_asset_exists <- function(root, rel, context) {
  if (!file.exists(file.path(root, rel))) stop("Registry asset path does not exist for ", context, ": ", rel, call. = FALSE)
}
