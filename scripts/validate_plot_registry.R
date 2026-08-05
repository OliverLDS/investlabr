#!/usr/bin/env Rscript

.script_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
.script_path <- normalizePath(sub("^--file=", "", .script_arg[[1L]]), mustWork = TRUE)
source(file.path(dirname(.script_path), "_node-common.R"))

usage <- function() {
  cat(paste(
    "Usage:",
    "  Rscript scripts/validate_plot_registry.R [--registry PATH] [--output-root PATH] [--skip-assets] [--repo-root PATH]",
    "",
    "Options:",
    "  --registry PATH     Registry JSON path. Default: output/publishing/plot-registry.json.",
    "  --output-root PATH  Root for relative asset validation. Default: output/publishing.",
    "  --skip-assets       Validate schema and entries without checking asset files.",
    "  --repo-root PATH    investlabr repository root. Default: inferred from this script.",
    "  -h, --help          Show this help message.",
    "",
    "Behavior and side effects:",
    "  - Reads and validates a schema 1.0, 2.0, or 3.0 registry plus its referenced local assets.",
    "  - Schema 3.0 validation enforces freshness fields and registry/render timestamp ordering.",
    "  - Does not write files.",
    "  - Emits one JSON validation result to stdout.",
    sep = "\n"
  ))
}

parse_args <- function(args) {
  out <- list(help = FALSE, registry = "output/publishing/plot-registry.json", output_root = "output/publishing", skip_assets = FALSE, repo_root = NULL)
  i <- 1L
  while (i <= length(args)) {
    arg <- args[[i]]
    if (arg %in% c("-h", "--help")) {
      out$help <- TRUE
      i <- i + 1L
      next
    }
    if (identical(arg, "--skip-assets")) {
      out$skip_assets <- TRUE
      i <- i + 1L
      next
    }
    if (!arg %in% c("--registry", "--output-root", "--repo-root")) {
      stop("Unknown argument: ", arg, call. = FALSE)
    }
    if (i == length(args)) stop("Missing value for ", arg, call. = FALSE)
    value <- args[[i + 1L]]
    if (identical(arg, "--registry")) out$registry <- value
    if (identical(arg, "--output-root")) out$output_root <- value
    if (identical(arg, "--repo-root")) out$repo_root <- value
    i <- i + 2L
  }
  out
}

args <- tryCatch(parse_args(commandArgs(trailingOnly = TRUE)), error = function(e) {
  node_emit_json(list(success = FALSE, valid = FALSE, plot_count = 0L, error = conditionMessage(e)))
  quit(status = 1L)
})
if (isTRUE(args$help)) {
  usage()
  quit(status = 0L)
}

tryCatch({
  repo_root <- if (is.null(args$repo_root)) node_repo_root(.script_path) else normalizePath(args$repo_root, mustWork = TRUE)
  registry_path <- node_resolve_path(args$registry, repo_root)
  output_root <- node_resolve_path(args$output_root, repo_root)
  node_load_investlabr(repo_root)
  result <- investlabr::brief_plot_registry_validate(
    registry = registry_path,
    output_root = output_root,
    require_assets = !isTRUE(args$skip_assets)
  )
  node_emit_json(c(list(success = TRUE, registry = normalizePath(registry_path, mustWork = TRUE)), result, list(error = NULL)))
}, error = function(e) {
  node_emit_json(list(success = FALSE, valid = FALSE, plot_count = 0L, error = conditionMessage(e)))
  quit(status = 1L)
})
