test_that("viz_style_get resolves named styles and contexts", {
  style <- viz_style_get(style = "macro_classic", context = "slide")

  testthat::expect_type(style, "list")
  testthat::expect_equal(style$name, "macro_classic")
  testthat::expect_equal(style$context, "slide")
  testthat::expect_true(all(c("accent", "accent2", "discrete", "line_width") %in% names(style)))
})

test_that("new visualization styles resolve", {
  style_names <- c(
    "briefing_serif",
    "strategy_explain",
    "institutional_blue",
    "policy_memo",
    "desk_monitor",
    "client_slide",
    "newswire_print"
  )

  for (style_name in style_names) {
    style <- viz_style_get(style = style_name, context = "report")
    testthat::expect_equal(style$name, style_name)
    testthat::expect_true(all(c("accent", "accent2", "paper", "grid") %in% names(style)))
  }
})

test_that("styled event plot returns a ggplot object", {
  DT <- data.table::data.table(
    datetime = seq.POSIXt(as.POSIXct("2024-01-01", tz = "UTC"), by = "day", length.out = 40),
    close = seq(100, 139),
    event_x = as.integer(seq_len(40) %% 10 == 0)
  )
  attr(DT, "inst_id") <- "TEST"

  res <- eval_event_performance(DT, "event_x", H = 1L:5L)
  p <- gen_plot_event_tsline_cum_ret(res, style = "presentation_bold", context = "slide")

  testthat::expect_s3_class(p, "ggplot")
})

test_that("viz_theme_apply appends configured compiler name to caption", {
  cfg_path <- tempfile(fileext = ".yaml")
  writeLines("plot_compiler_name: Oliver Zhou", cfg_path)

  old_cfg <- Sys.getenv("INVESTLABR_CONFIG", unset = NA_character_)
  on.exit({
    if (is.na(old_cfg)) {
      Sys.unsetenv("INVESTLABR_CONFIG")
    } else {
      Sys.setenv(INVESTLABR_CONFIG = old_cfg)
    }
    unlink(cfg_path)
  }, add = TRUE)
  Sys.setenv(INVESTLABR_CONFIG = cfg_path)

  p <- ggplot2::ggplot(data.frame(x = 1:3, y = 1:3), ggplot2::aes(x, y)) +
    ggplot2::geom_line() +
    ggplot2::labs(caption = "Source: Demo")

  styled <- viz_theme_apply(p, style = "macro_classic", context = "report")

  testthat::expect_match(styled$labels$caption, "Source: Demo", fixed = TRUE)
  testthat::expect_match(styled$labels$caption, "Compiled by: Oliver Zhou", fixed = TRUE)
})

test_that("viz_theme_apply can suppress configured compiler caption", {
  cfg_path <- tempfile(fileext = ".yaml")
  writeLines("plot_compiler_name: Oliver Zhou", cfg_path)

  old_cfg <- Sys.getenv("INVESTLABR_CONFIG", unset = NA_character_)
  on.exit({
    if (is.na(old_cfg)) {
      Sys.unsetenv("INVESTLABR_CONFIG")
    } else {
      Sys.setenv(INVESTLABR_CONFIG = old_cfg)
    }
    unlink(cfg_path)
  }, add = TRUE)
  Sys.setenv(INVESTLABR_CONFIG = cfg_path)

  p <- ggplot2::ggplot(data.frame(x = 1:3, y = 1:3), ggplot2::aes(x, y)) +
    ggplot2::geom_line()

  styled <- viz_theme_apply(p, style = "macro_classic", context = "report", show_compiler = FALSE)

  testthat::expect_true(is.null(styled$labels$caption) || !nzchar(styled$labels$caption))
})

test_that("grid wrapper appends configured compiler caption to footer", {
  cfg_path <- tempfile(fileext = ".yaml")
  writeLines("plot_compiler_name: Oliver Zhou", cfg_path)

  old_cfg <- Sys.getenv("INVESTLABR_CONFIG", unset = NA_character_)
  on.exit({
    if (is.na(old_cfg)) {
      Sys.unsetenv("INVESTLABR_CONFIG")
    } else {
      Sys.setenv(INVESTLABR_CONFIG = old_cfg)
    }
    unlink(cfg_path)
  }, add = TRUE)
  Sys.setenv(INVESTLABR_CONFIG = cfg_path)

  p <- ggplot2::ggplot(data.frame(x = 1:3, y = 1:3), ggplot2::aes(x, y)) + ggplot2::geom_line()
  arranged <- gen_grid_of_plots_with_labels(
    plots = list(p),
    n_rows = 1,
    n_cols = 1,
    bottom = "Source: Demo",
    style = "macro_classic",
    context = "report"
  )

  grob_names <- vapply(arranged$grobs, function(g) paste(class(g), collapse = "/"), character(1))
  text_idx <- which(grob_names == "text/grob/gDesc")
  grob_labels <- vapply(arranged$grobs[text_idx], function(g) as.character(g$label), character(1))

  testthat::expect_true(any(grepl("Source: Demo", grob_labels, fixed = TRUE)))
  testthat::expect_true(any(grepl("Compiled by: Oliver Zhou", grob_labels, fixed = TRUE)))
})

test_that("grid wrapper wraps long footer text", {
  p <- ggplot2::ggplot(data.frame(x = 1:3, y = 1:3), ggplot2::aes(x, y)) + ggplot2::geom_line()
  long_footer <- paste(rep("This footer should wrap automatically when it gets too long for a single line.", 4), collapse = " ")

  arranged <- gen_grid_of_plots_with_labels(
    plots = list(p),
    n_rows = 1,
    n_cols = 1,
    bottom = long_footer,
    style = "macro_classic",
    context = "report",
    show_compiler = FALSE
  )

  grob_names <- vapply(arranged$grobs, function(g) paste(class(g), collapse = "/"), character(1))
  text_idx <- which(grob_names == "text/grob/gDesc")
  grob_labels <- vapply(arranged$grobs[text_idx], function(g) as.character(g$label), character(1))

  testthat::expect_true(any(grepl("\n", grob_labels, fixed = TRUE)))
})
