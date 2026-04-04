.investlabr_config_path <- function(path = NULL) {
  if (!is.null(path)) return(path)
  Sys.getenv("INVESTLABR_CONFIG", unset = "")
}

.investlabr_config_read <- function(path = NULL) {
  path <- .investlabr_config_path(path)
  if (!nzchar(path) || !file.exists(path)) return(list())
  if (!requireNamespace("yaml", quietly = TRUE)) return(list())

  cfg <- tryCatch(
    yaml::read_yaml(path),
    error = function(e) list()
  )

  if (is.null(cfg)) list() else cfg
}

.investlabr_plot_compiler_name <- function(path = NULL) {
  cfg <- .investlabr_config_read(path)
  compiler_name <- cfg[["plot_compiler_name"]]

  if (is.null(compiler_name) && is.list(cfg[["plot"]])) {
    compiler_name <- cfg[["plot"]][["compiler_name"]]
  }

  if (is.null(compiler_name) || !nzchar(trimws(as.character(compiler_name)[1]))) {
    return("")
  }

  trimws(as.character(compiler_name)[1])
}

.investlabr_compiler_caption <- function(existing_caption = NULL, path = NULL) {
  compiler_name <- .investlabr_plot_compiler_name(path = path)
  if (!nzchar(compiler_name)) return(existing_caption)

  compiler_caption <- paste0("Compiled by: ", compiler_name)
  if (is.null(existing_caption) || !nzchar(existing_caption)) {
    return(compiler_caption)
  }
  if (grepl(compiler_caption, existing_caption, fixed = TRUE)) {
    return(existing_caption)
  }

  paste(existing_caption, compiler_caption, sep = "\n")
}
