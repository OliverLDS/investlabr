#!/usr/bin/env Rscript

.script_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
.script_path <- normalizePath(sub("^--file=", "", .script_arg[[1L]]), mustWork = TRUE)
source(file.path(dirname(.script_path), "_node-common.R"))

usage <- function() {
  cat(paste(
    "Usage:",
    "  Rscript scripts/render_plot_assets.R [--ids ID1,ID2] [--output-root PATH] [--repo-root PATH]",
    "",
    "Options:",
    "  --ids LIST          Optional comma-separated plot ids. Default: all configured assets.",
    "  --output-root PATH  Publishing output root. Default: output/publishing.",
    "  --repo-root PATH    investlabr repository root. Default: inferred from this script.",
    "  -h, --help          Show this help message.",
    "",
    "Behavior and side effects:",
    "  - Reads local investdatar caches through selected gallery examples.",
    "  - Writes primary plots under OUTPUT_ROOT/plots/.",
    "  - Writes thumbnails under OUTPUT_ROOT/thumbnails/.",
    "  - Writes run-local freshness metadata under OUTPUT_ROOT/resolved/.",
    "  - Copies curated style/context previews into the same publishing tree.",
    "  - Emits one JSON result to stdout; progress and package messages use stderr.",
    sep = "\n"
  ))
}

parse_args <- function(args) {
  out <- list(help = FALSE, ids = NULL, output_root = "output/publishing", repo_root = NULL)
  i <- 1L
  while (i <= length(args)) {
    arg <- args[[i]]
    if (arg %in% c("-h", "--help")) {
      out$help <- TRUE
      i <- i + 1L
      next
    }
    if (!arg %in% c("--ids", "--output-root", "--repo-root")) {
      stop("Unknown argument: ", arg, call. = FALSE)
    }
    if (i == length(args)) stop("Missing value for ", arg, call. = FALSE)
    value <- args[[i + 1L]]
    if (identical(arg, "--ids")) out$ids <- value
    if (identical(arg, "--output-root")) out$output_root <- value
    if (identical(arg, "--repo-root")) out$repo_root <- value
    i <- i + 2L
  }
  out
}

render_specs <- list(
  list(id = "fred-liquidity-tightness-dashboard", script = "macro-monitor/real-data-fred-liquidity-tightness-dashboard.R", subdir = "macro", width = 14, height = 10),
  list(id = "fred-inflation-labor-dashboard", script = "macro-monitor/real-data-fred-inflation-labor-dashboard.R", subdir = "macro", width = 14, height = 10),
  list(id = "fred-rate-shock-persistence-board", script = "macro-forecast/real-data-fred-rate-shock-persistence-board.R", subdir = "macro", width = 14, height = 10),
  list(id = "yahoo-cross-asset-event-board", script = "cross-asset/real-data-yahoo-cross-asset-event-board.R", subdir = "markets", width = 14, height = 10),
  list(id = "fred-fomc-plumbing-board", script = "macro-monitor/real-data-fred-fomc-plumbing-board.R", subdir = "macro", width = 14, height = 10),
  list(id = "fred-balance-sheet-mirror-board", script = "macro-monitor/real-data-fred-balance-sheet-mirror-board.R", subdir = "macro", width = 14, height = 10),
  list(id = "macro-factor-heatmap", script = "cross-asset/real-data-macro-factor-heatmap.R", subdir = "markets", width = 14, height = 10)
)

preview_specs <- list(
  list(id = "context-dashboard", image = "context-dashboard.svg", thumbnail = "context-dashboard.png"),
  list(id = "context-report", image = "context-report.svg", thumbnail = "context-report.png"),
  list(id = "context-slide", image = "context-slide.svg", thumbnail = "context-slide.png"),
  list(id = "viz-style-gallery", image = "viz-style-gallery.png", thumbnail = "viz-style-gallery.png")
)

resolve_plot_object <- function(env, sourced_value) {
  for (nm in c("dashboard", "board", "final_board", "plot_obj", "p")) {
    if (exists(nm, envir = env, inherits = FALSE)) {
      return(get(nm, envir = env, inherits = FALSE))
    }
  }
  if (!is.null(sourced_value) && !inherits(sourced_value, "data.frame")) return(sourced_value)
  stop("Could not resolve a plot object from sourced gallery script.", call. = FALSE)
}

source_gallery_plot <- function(path) {
  env <- new.env(parent = globalenv())
  sourced_value <- NULL
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  invisible(capture.output({
    result <- source(path, local = env)
    sourced_value <- result$value
  }))
  if (!exists("artifact_freshness", envir = env, inherits = FALSE)) {
    stop("Gallery recipe did not expose `artifact_freshness`.", call. = FALSE)
  }
  list(
    plot = resolve_plot_object(env, sourced_value),
    freshness = get("artifact_freshness", envir = env, inherits = FALSE)
  )
}

utc_timestamp <- function(x = Sys.time()) {
  format(as.POSIXct(x, tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
}

load_tracked_meta <- function(meta_root, id) {
  path <- file.path(meta_root, paste0(id, ".yaml"))
  if (!file.exists(path)) stop("Tracked metadata sidecar not found: ", path, call. = FALSE)
  meta <- yaml::read_yaml(path)
  if (is.null(meta$metadata_updated_at) || is.null(meta$time_indexed)) {
    stop("Tracked metadata lacks `metadata_updated_at` or `time_indexed`: ", id, call. = FALSE)
  }
  list(path = path, metadata = meta)
}

write_resolved_meta <- function(output_root, tracked, rendered_at, freshness) {
  if (!is.list(freshness) || is.null(freshness$data_as_of) || is.null(freshness$rule)) {
    stop("Time-indexed recipe freshness must contain `data_as_of` and `rule`.", call. = FALSE)
  }
  data_as_of <- as.character(freshness$data_as_of)
  parsed_data_date <- tryCatch(as.Date(data_as_of), error = function(e) as.Date(NA))
  if (length(data_as_of) != 1L || is.na(parsed_data_date) || format(parsed_data_date, "%Y-%m-%d") != data_as_of) {
    stop("Recipe returned an invalid `data_as_of` value.", call. = FALSE)
  }
  resolved <- list(
    id = tracked$id,
    rendered_at = rendered_at,
    data_as_of = data_as_of,
    metadata_updated_at = as.character(tracked$metadata_updated_at),
    time_indexed = isTRUE(tracked$time_indexed),
    data_as_of_rule = as.character(freshness$rule)
  )
  dir <- file.path(output_root, "resolved")
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  jsonlite::write_json(
    resolved,
    file.path(dir, paste0(tracked$id, ".json")),
    auto_unbox = TRUE,
    pretty = TRUE,
    null = "null"
  )
  resolved
}

write_preview_resolved_meta <- function(output_root, tracked, rendered_at) {
  resolved <- list(
    id = tracked$id,
    rendered_at = rendered_at,
    data_as_of = NULL,
    metadata_updated_at = as.character(tracked$metadata_updated_at),
    time_indexed = FALSE,
    data_as_of_rule = "not_time_indexed"
  )
  dir <- file.path(output_root, "resolved")
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  jsonlite::write_json(resolved, file.path(dir, paste0(tracked$id, ".json")), auto_unbox = TRUE, pretty = TRUE, null = "null")
  resolved
}

args <- tryCatch(parse_args(commandArgs(trailingOnly = TRUE)), error = function(e) {
  node_emit_json(list(success = FALSE, output_root = NULL, rendered = list(), error = conditionMessage(e)))
  quit(status = 1L)
})
if (isTRUE(args$help)) {
  usage()
  quit(status = 0L)
}

tryCatch({
  repo_root <- if (is.null(args$repo_root)) node_repo_root(.script_path) else normalizePath(args$repo_root, mustWork = TRUE)
  output_root <- node_resolve_path(args$output_root, repo_root)
  gallery_root <- file.path(repo_root, "inst", "gallery")
  asset_root <- file.path(gallery_root, "assets")
  meta_root <- file.path(repo_root, "config", "publishing", "plots")
  tracked_meta_files <- sort(Sys.glob(file.path(meta_root, "*.yaml")))
  tracked_meta_hashes <- tools::md5sum(tracked_meta_files)
  selected_ids <- node_parse_csv(args$ids)
  known_ids <- c(
    vapply(render_specs, `[[`, character(1), "id"),
    vapply(preview_specs, `[[`, character(1), "id")
  )
  unknown_ids <- setdiff(selected_ids, known_ids)
  if (length(unknown_ids) > 0L) {
    stop("Unknown plot ids: ", paste(unknown_ids, collapse = ", "), call. = FALSE)
  }

  node_load_investlabr(repo_root)
  rendered <- list()
  for (spec in render_specs) {
    if (length(selected_ids) > 0L && !spec$id %in% selected_ids) next
    plot_dir <- file.path(output_root, "plots", spec$subdir)
    thumb_dir <- file.path(output_root, "thumbnails", spec$subdir)
    dir.create(plot_dir, recursive = TRUE, showWarnings = FALSE)
    dir.create(thumb_dir, recursive = TRUE, showWarnings = FALSE)
    plot_path <- file.path(plot_dir, paste0(spec$id, ".svg"))
    thumb_path <- file.path(thumb_dir, paste0(spec$id, ".png"))
    recipe <- source_gallery_plot(file.path(gallery_root, spec$script))
    ggplot2::ggsave(plot_path, plot = recipe$plot, width = spec$width, height = spec$height, dpi = 144)
    ggplot2::ggsave(thumb_path, plot = recipe$plot, width = 5.5, height = 3.8, dpi = 144)
    rendered_at <- utc_timestamp()
    tracked <- load_tracked_meta(meta_root, spec$id)$metadata
    resolved <- write_resolved_meta(output_root, tracked, rendered_at, recipe$freshness)
    rendered[[length(rendered) + 1L]] <- list(
      id = spec$id,
      plot_image = normalizePath(plot_path, mustWork = TRUE),
      thumbnail = normalizePath(thumb_path, mustWork = TRUE),
      rendered_at = resolved$rendered_at,
      data_as_of = resolved$data_as_of,
      metadata_updated_at = resolved$metadata_updated_at,
      data_as_of_rule = resolved$data_as_of_rule
    )
    message("Rendered ", spec$id)
  }

  for (spec in preview_specs) {
    if (length(selected_ids) > 0L && !spec$id %in% selected_ids) next
    plot_dir <- file.path(output_root, "plots", "markets")
    thumb_dir <- file.path(output_root, "thumbnails", "markets")
    dir.create(plot_dir, recursive = TRUE, showWarnings = FALSE)
    dir.create(thumb_dir, recursive = TRUE, showWarnings = FALSE)
    plot_path <- file.path(plot_dir, spec$image)
    thumb_path <- file.path(thumb_dir, spec$thumbnail)
    if (!file.copy(file.path(asset_root, spec$image), plot_path, overwrite = TRUE)) {
      stop("Could not publish curated preview: ", spec$image, call. = FALSE)
    }
    if (!file.copy(file.path(asset_root, spec$thumbnail), thumb_path, overwrite = TRUE)) {
      stop("Could not publish curated thumbnail: ", spec$thumbnail, call. = FALSE)
    }
    rendered_at <- utc_timestamp()
    tracked <- load_tracked_meta(meta_root, spec$id)$metadata
    resolved <- write_preview_resolved_meta(output_root, tracked, rendered_at)
    rendered[[length(rendered) + 1L]] <- list(
      id = spec$id,
      plot_image = normalizePath(plot_path, mustWork = TRUE),
      thumbnail = normalizePath(thumb_path, mustWork = TRUE),
      rendered_at = resolved$rendered_at,
      data_as_of = NULL,
      metadata_updated_at = resolved$metadata_updated_at,
      data_as_of_rule = resolved$data_as_of_rule
    )
  }

  if (!identical(unname(tools::md5sum(tracked_meta_files)), unname(tracked_meta_hashes))) {
    stop("Rendering modified tracked publishing metadata, which is forbidden.", call. = FALSE)
  }

  node_emit_json(list(
    success = TRUE,
    output_root = normalizePath(output_root, mustWork = TRUE),
    rendered = rendered,
    error = NULL
  ))
}, error = function(e) {
  node_emit_json(list(success = FALSE, output_root = args$output_root, rendered = list(), error = conditionMessage(e)))
  quit(status = 1L)
})
