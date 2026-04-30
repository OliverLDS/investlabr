.gallery_file <- tryCatch(normalizePath(sys.frame(1)$ofile), error = function(e) NA_character_)
.gallery_dir <- if (!is.na(.gallery_file)) dirname(.gallery_file) else file.path(getwd(), "inst/gallery")

library(investlabr)

output_root <- file.path(.gallery_dir, "output")
meta_dir <- file.path(output_root, "meta")
registry_path <- file.path(output_root, "alphasync-plot-registry.json")

dir.create(file.path(output_root, "plots"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(output_root, "thumbnails"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(output_root, "html"), recursive = TRUE, showWarnings = FALSE)
dir.create(meta_dir, recursive = TRUE, showWarnings = FALSE)

reg <- investlabr::brief_plot_registry_write_from_meta(
  meta_dir = meta_dir,
  path = registry_path,
  output_root = output_root,
  published_only = FALSE,
  pretty = TRUE
)

cat(
  paste0(
    "Wrote AlphaSync plot registry to ", registry_path, ".\n",
    "Entries: ", length(reg$plots), "\n"
  )
)
