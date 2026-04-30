#' Build one AlphaSync-compatible plot-registry entry
#'
#' @param id Stable machine-safe plot identifier in lowercase kebab-case.
#' @param title Reader-facing plot title.
#' @param dashboard High-level dashboard grouping. One of
#'   \code{"macro"}, \code{"markets"}, \code{"crypto"}, or
#'   \code{"strategies"}.
#' @param asset_class Asset-class label such as \code{"Rates"} or
#'   \code{"Liquidity"}.
#' @param indicator_family Indicator family label such as
#'   \code{"Yield Curve"}.
#' @param region Region label.
#' @param frequency Frequency label.
#' @param source Source label such as \code{"FRED"}.
#' @param last_updated ISO date string or \code{Date}.
#' @param published Logical flag for production-ready output.
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
#' @param featured Logical flag used by downstream publishers.
#' @param sort_priority Numeric sort priority; higher values can be pinned first.
#' @param plot_html Relative path to an optional HTML artifact. Use \code{""}
#'   when none exists.
#' @param canonical_slug Optional canonical slug. Defaults to \code{id}.
#' @param related_ids Optional character vector of related plot ids.
#' @param podcast_topics Optional character vector of podcast-topic labels.
#' @param report_topics Optional character vector of report-topic labels.
#' @param compliance Named list. Defaults to educational-only and
#'   not-investment-advice flags.
#'
#' @return Named list representing one registry entry.
#' @export
brief_plot_registry_entry <- function(
  id,
  title,
  dashboard,
  asset_class,
  indicator_family,
  region,
  frequency,
  source,
  last_updated,
  published,
  plot_image,
  thumbnail,
  tags,
  subtitle = "",
  summary = "",
  description_md = "",
  section = "",
  source_detail = character(),
  featured = FALSE,
  sort_priority = 0,
  plot_html = "",
  canonical_slug = NULL,
  related_ids = character(),
  podcast_topics = character(),
  report_topics = character(),
  compliance = list(
    educational_only = TRUE,
    not_investment_advice = TRUE
  )
) {
  if (is.null(canonical_slug) || !nzchar(canonical_slug)) canonical_slug <- id

  entry <- list(
    id = .brief_registry_scalar_chr(id),
    title = .brief_registry_scalar_chr(title),
    subtitle = .brief_registry_scalar_chr(subtitle),
    summary = .brief_registry_scalar_chr(summary),
    description_md = .brief_registry_scalar_chr(description_md),
    dashboard = .brief_registry_scalar_chr(dashboard),
    section = .brief_registry_scalar_chr(section),
    asset_class = .brief_registry_scalar_chr(asset_class),
    indicator_family = .brief_registry_scalar_chr(indicator_family),
    region = .brief_registry_scalar_chr(region),
    frequency = .brief_registry_scalar_chr(frequency),
    source = .brief_registry_scalar_chr(source),
    source_detail = .brief_registry_chr(source_detail),
    tags = .brief_registry_chr(tags),
    last_updated = .brief_registry_format_date(last_updated),
    published = isTRUE(published),
    featured = isTRUE(featured),
    sort_priority = as.integer(sort_priority),
    plot_image = .brief_registry_normalize_rel_path(plot_image),
    thumbnail = .brief_registry_normalize_rel_path(thumbnail),
    plot_html = .brief_registry_normalize_rel_path(plot_html, allow_empty = TRUE),
    canonical_slug = .brief_registry_scalar_chr(canonical_slug),
    related_ids = .brief_registry_chr(related_ids),
    podcast_topics = .brief_registry_chr(podcast_topics),
    report_topics = .brief_registry_chr(report_topics),
    compliance = .brief_registry_normalize_compliance(compliance)
  )

  .brief_registry_validate_entry(entry)
}

#' Write an AlphaSync-compatible plot registry to JSON
#'
#' @param plots List of registry entries, typically created with
#'   \code{brief_plot_registry_entry()}.
#' @param path Output JSON path.
#' @param schema_version Schema version string.
#' @param generated_at Generation timestamp. Defaults to current UTC time.
#' @param source_system Source-system label.
#' @param base_path_mode Path-mode label. Defaults to \code{"relative"}.
#' @param published_only If \code{TRUE}, only keep entries with
#'   \code{published = TRUE}.
#' @param pretty If \code{TRUE}, write pretty JSON.
#'
#' @return Invisibly returns the registry object written to disk.
#' @export
brief_plot_registry_write <- function(
  plots,
  path = file.path("inst", "gallery", "output", "alphasync-plot-registry.json"),
  schema_version = "1.0",
  generated_at = Sys.time(),
  source_system = "investlabr",
  base_path_mode = "relative",
  published_only = FALSE,
  pretty = TRUE
) {
  if (is.null(plots)) plots <- list()
  if (!is.list(plots)) stop("`plots` must be a list.", call. = FALSE)

  normalized_plots <- lapply(plots, .brief_registry_validate_entry)
  normalized_plots <- lapply(normalized_plots, .brief_registry_prepare_for_json)
  if (isTRUE(published_only)) {
    normalized_plots <- Filter(function(x) isTRUE(x$published), normalized_plots)
  }

  reg <- list(
    schema_version = as.character(schema_version),
    generated_at = .brief_registry_format_timestamp(generated_at),
    source_system = as.character(source_system),
    base_path_mode = as.character(base_path_mode),
    plots = normalized_plots
  )

  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  jsonlite::write_json(reg, path = path, auto_unbox = TRUE, pretty = pretty, null = "null")
  invisible(reg)
}

#' Build and write an AlphaSync-compatible plot registry from YAML sidecars
#'
#' @param meta_dir Directory containing one-plot YAML sidecars.
#' @param path Output JSON path.
#' @param output_root Root directory against which relative asset paths are
#'   validated.
#' @param schema_version Schema version string.
#' @param source_system Source-system label.
#' @param base_path_mode Path-mode label. Defaults to \code{"relative"}.
#' @param published_only If \code{TRUE}, only keep entries with
#'   \code{published = TRUE}.
#' @param pretty If \code{TRUE}, write pretty JSON.
#'
#' @return Invisibly returns the registry object written to disk.
#' @export
brief_plot_registry_write_from_meta <- function(
  meta_dir = file.path("inst", "gallery", "output", "meta"),
  path = file.path("inst", "gallery", "output", "alphasync-plot-registry.json"),
  output_root = dirname(path),
  schema_version = "1.0",
  source_system = "investlabr",
  base_path_mode = "relative",
  published_only = FALSE,
  pretty = TRUE
) {
  plots <- .brief_registry_load_meta_dir(meta_dir = meta_dir, output_root = output_root)
  brief_plot_registry_write(
    plots = plots,
    path = path,
    schema_version = schema_version,
    generated_at = Sys.time(),
    source_system = source_system,
    base_path_mode = base_path_mode,
    published_only = published_only,
    pretty = pretty
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

    entry <- brief_plot_registry_entry(
      id = entry$id,
      title = entry$title,
      subtitle = .brief_registry_value_or_default(entry$subtitle, ""),
      summary = .brief_registry_value_or_default(entry$summary, ""),
      description_md = .brief_registry_value_or_default(entry$description_md, ""),
      dashboard = entry$dashboard,
      section = .brief_registry_value_or_default(entry$section, ""),
      asset_class = entry$asset_class,
      indicator_family = entry$indicator_family,
      region = entry$region,
      frequency = entry$frequency,
      source = entry$source,
      source_detail = .brief_registry_chr(entry$source_detail),
      tags = .brief_registry_chr(entry$tags),
      last_updated = entry$last_updated,
      published = entry$published,
      featured = .brief_registry_value_or_default(entry$featured, FALSE),
      sort_priority = .brief_registry_value_or_default(entry$sort_priority, 0L),
      plot_image = entry$plot_image,
      thumbnail = entry$thumbnail,
      plot_html = .brief_registry_value_or_default(entry$plot_html, ""),
      canonical_slug = .brief_registry_value_or_default(entry$canonical_slug, NULL),
      related_ids = .brief_registry_chr(entry$related_ids),
      podcast_topics = .brief_registry_chr(entry$podcast_topics),
      report_topics = .brief_registry_chr(entry$report_topics),
      compliance = .brief_registry_value_or_default(
        entry$compliance,
        list(educational_only = TRUE, not_investment_advice = TRUE)
      )
    )

    .brief_registry_assert_asset_exists(output_root, entry$plot_image, meta_file)
    .brief_registry_assert_asset_exists(output_root, entry$thumbnail, meta_file)
    if (nzchar(entry$plot_html)) {
      .brief_registry_assert_asset_exists(output_root, entry$plot_html, meta_file)
    }

    entry
  })
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
  out[[1]]
}

.brief_registry_format_date <- function(x) {
  if (inherits(x, "Date")) return(format(x, "%Y-%m-%d"))
  out <- as.character(x)
  if (!grepl("^\\d{4}-\\d{2}-\\d{2}$", out)) {
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
  array_fields <- c("source_detail", "tags", "related_ids", "podcast_topics", "report_topics")
  for (nm in array_fields) {
    entry[[nm]] <- I(as.list(unname(entry[[nm]])))
  }
  entry
}

.brief_registry_validate_entry <- function(entry) {
  required_fields <- c(
    "id", "title", "dashboard", "asset_class", "indicator_family", "region",
    "frequency", "source", "last_updated", "published", "plot_image",
    "thumbnail", "tags"
  )
  missing_fields <- required_fields[!required_fields %in% names(entry)]
  if (length(missing_fields) > 0L) {
    stop("Missing required registry fields: ", paste(missing_fields, collapse = ", "), call. = FALSE)
  }

  for (nm in c("id", "title", "dashboard", "asset_class", "indicator_family",
               "region", "frequency", "source", "plot_image", "thumbnail")) {
    if (!is.character(entry[[nm]]) || length(entry[[nm]]) != 1L || !nzchar(entry[[nm]])) {
      stop("`", nm, "` must be a non-empty length-1 character value.", call. = FALSE)
    }
  }
  if (!grepl("^[a-z0-9]+(?:-[a-z0-9]+)*$", entry$id)) {
    stop("`id` must be lowercase kebab-case.", call. = FALSE)
  }
  if (!grepl("^[a-z0-9]+(?:-[a-z0-9]+)*$", entry$canonical_slug)) {
    stop("`canonical_slug` must be lowercase kebab-case.", call. = FALSE)
  }

  allowed_dashboards <- c("macro", "markets", "crypto", "strategies")
  if (!entry$dashboard %in% allowed_dashboards) {
    stop("`dashboard` must be one of: ", paste(allowed_dashboards, collapse = ", "), call. = FALSE)
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
  entry$podcast_topics <- .brief_registry_chr(entry$podcast_topics)
  entry$report_topics <- .brief_registry_chr(entry$report_topics)
  entry$published <- isTRUE(entry$published)
  entry$featured <- isTRUE(entry$featured)
  entry$sort_priority <- as.integer(entry$sort_priority)
  entry$compliance <- .brief_registry_normalize_compliance(entry$compliance)

  if (length(entry$tags) == 0L || any(!nzchar(entry$tags))) {
    stop("`tags` must be a non-empty flat character vector.", call. = FALSE)
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
