.gallery_file <- tryCatch(normalizePath(sys.frame(1)$ofile), error = function(e) NA_character_)
.gallery_dir <- if (!is.na(.gallery_file)) dirname(.gallery_file) else file.path(getwd(), "inst/gallery")

library(ggplot2)
library(investlabr)

output_root <- file.path(.gallery_dir, "output")

render_specs <- list(
  list(
    script = "real-data-fred-liquidity-tightness-dashboard.R",
    id = "fred-liquidity-tightness-dashboard",
    filename = "fred-liquidity-tightness-dashboard",
    subdir = "macro",
    width = 14,
    height = 10
  ),
  list(
    script = "real-data-fred-inflation-labor-dashboard.R",
    id = "fred-inflation-labor-dashboard",
    filename = "fred-inflation-labor-dashboard",
    subdir = "macro",
    width = 14,
    height = 10
  ),
  list(
    script = "real-data-fred-rate-shock-persistence-board.R",
    id = "fred-rate-shock-persistence-board",
    filename = "fred-rate-shock-persistence-board",
    subdir = "macro",
    width = 14,
    height = 10
  ),
  list(
    script = "real-data-yahoo-cross-asset-event-board.R",
    id = "yahoo-cross-asset-event-board",
    filename = "yahoo-cross-asset-event-board",
    subdir = "markets",
    width = 14,
    height = 10
  ),
  list(
    script = "real-data-fred-fomc-plumbing-board.R",
    id = "fred-fomc-plumbing-board",
    filename = "fred-fomc-plumbing-board",
    subdir = "macro",
    width = 14,
    height = 10
  ),
  list(
    script = "real-data-fred-balance-sheet-mirror-board.R",
    id = "fred-balance-sheet-mirror-board",
    filename = "fred-balance-sheet-mirror-board",
    subdir = "macro",
    width = 14,
    height = 10
  ),
  list(
    script = "real-data-macro-factor-heatmap.R",
    id = "macro-factor-heatmap",
    filename = "macro-factor-heatmap",
    subdir = "markets",
    width = 14,
    height = 10
  )
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

for (spec in render_specs) {
  plot_dir <- file.path(output_root, "plots", spec$subdir)
  thumb_dir <- file.path(output_root, "thumbnails", spec$subdir)
  dir.create(plot_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(thumb_dir, recursive = TRUE, showWarnings = FALSE)

  env <- new.env(parent = globalenv())
  res <- source(file.path(.gallery_dir, spec$script), local = env)
  plot_obj <- resolve_plot_object(env, res$value)

  svg_path <- file.path(plot_dir, paste0(spec$filename, ".svg"))
  png_path <- file.path(thumb_dir, paste0(spec$filename, ".png"))

  ggplot2::ggsave(svg_path, plot = plot_obj, width = spec$width, height = spec$height, dpi = 144)
  ggplot2::ggsave(png_path, plot = plot_obj, width = 5.5, height = 3.8, dpi = 144)

  cat("Rendered ", spec$id, " -> ", svg_path, "\n", sep = "")
}
