node_script_path <- function() {
  file_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  if (length(file_arg) == 0L) {
    stop("This task node must be run with Rscript.", call. = FALSE)
  }
  normalizePath(sub("^--file=", "", file_arg[[1L]]), mustWork = TRUE)
}

node_repo_root <- function(script_path = node_script_path()) {
  normalizePath(file.path(dirname(script_path), ".."), mustWork = TRUE)
}

node_resolve_path <- function(path, root) {
  if (grepl("^(/|[A-Za-z]:[/\\\\])", path)) {
    return(normalizePath(path, mustWork = FALSE))
  }
  normalizePath(file.path(root, path), mustWork = FALSE)
}

node_parse_csv <- function(x) {
  if (is.null(x) || !length(x) || !nzchar(x)) return(character())
  values <- trimws(unlist(strsplit(x, ",", fixed = TRUE), use.names = FALSE))
  unique(values[nzchar(values)])
}

node_emit_json <- function(value) {
  cat(jsonlite::toJSON(value, auto_unbox = TRUE, null = "null", dataframe = "rows"), "\n")
}

node_load_investlabr <- function(repo_root) {
  if (requireNamespace("pkgload", quietly = TRUE)) {
    pkgload::load_all(repo_root, quiet = TRUE)
  } else {
    suppressPackageStartupMessages(library(investlabr))
  }
}
