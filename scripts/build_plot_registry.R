#!/usr/bin/env Rscript

.script_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
.script_path <- normalizePath(sub("^--file=", "", .script_arg[[1L]]), mustWork = TRUE)
source(file.path(dirname(.script_path), "_node-common.R"))

usage <- function() {
  cat(paste(
    "Usage:",
    "  Rscript scripts/build_plot_registry.R [--metadata-dir PATH] [--output-root PATH] [--registry PATH] [--include-drafts] [--repo-root PATH]",
    "",
    "Options:",
    "  --metadata-dir PATH  YAML sidecar directory. Default: config/publishing/plots.",
    "  --output-root PATH   Asset root used to validate relative paths. Default: output/publishing.",
    "  --registry PATH      Registry JSON path. Default: OUTPUT_ROOT/plot-registry.json.",
    "  --include-drafts     Include draft and archived entries. Default: ready entries only.",
    "  --repo-root PATH     investlabr repository root. Default: inferred from this script.",
    "  -h, --help           Show this help message.",
    "",
    "Behavior and side effects:",
    "  - Reads tracked YAML plot metadata and validates referenced local assets.",
    "  - Writes one consumer-neutral research-artifact JSON registry.",
    "  - Emits one JSON result to stdout.",
    sep = "\n"
  ))
}

parse_args <- function(args) {
  out <- list(help = FALSE, metadata_dir = "config/publishing/plots", output_root = "output/publishing", registry = NULL, include_drafts = FALSE, repo_root = NULL)
  i <- 1L
  while (i <= length(args)) {
    arg <- args[[i]]
    if (arg %in% c("-h", "--help")) {
      out$help <- TRUE
      i <- i + 1L
      next
    }
    if (identical(arg, "--include-drafts")) {
      out$include_drafts <- TRUE
      i <- i + 1L
      next
    }
    if (!arg %in% c("--metadata-dir", "--output-root", "--registry", "--repo-root")) {
      stop("Unknown argument: ", arg, call. = FALSE)
    }
    if (i == length(args)) stop("Missing value for ", arg, call. = FALSE)
    value <- args[[i + 1L]]
    if (identical(arg, "--metadata-dir")) out$metadata_dir <- value
    if (identical(arg, "--output-root")) out$output_root <- value
    if (identical(arg, "--registry")) out$registry <- value
    if (identical(arg, "--repo-root")) out$repo_root <- value
    i <- i + 2L
  }
  out
}

args <- tryCatch(parse_args(commandArgs(trailingOnly = TRUE)), error = function(e) {
  node_emit_json(list(success = FALSE, registry = NULL, plot_count = 0L, error = conditionMessage(e)))
  quit(status = 1L)
})
if (isTRUE(args$help)) {
  usage()
  quit(status = 0L)
}

tryCatch({
  repo_root <- if (is.null(args$repo_root)) node_repo_root(.script_path) else normalizePath(args$repo_root, mustWork = TRUE)
  metadata_dir <- node_resolve_path(args$metadata_dir, repo_root)
  output_root <- node_resolve_path(args$output_root, repo_root)
  registry_path <- if (is.null(args$registry)) {
    file.path(output_root, "plot-registry.json")
  } else {
    node_resolve_path(args$registry, repo_root)
  }
  node_load_investlabr(repo_root)
  registry <- investlabr::brief_plot_registry_write_from_meta(
    meta_dir = metadata_dir,
    path = registry_path,
    output_root = output_root,
    ready_only = !isTRUE(args$include_drafts),
    pretty = TRUE
  )
  node_emit_json(list(
    success = TRUE,
    registry = normalizePath(registry_path, mustWork = TRUE),
    plot_count = length(registry$plots),
    ready_only = !isTRUE(args$include_drafts),
    error = NULL
  ))
}, error = function(e) {
  node_emit_json(list(success = FALSE, registry = args$registry, plot_count = 0L, error = conditionMessage(e)))
  quit(status = 1L)
})
