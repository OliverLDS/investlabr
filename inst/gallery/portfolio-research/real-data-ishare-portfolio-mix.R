library(data.table)
library(ggplot2)
library(investdatar)
library(investlabr)

tickers <- c("IVV", "IEFA", "AGG", "IEMG", "IAU", "SLV", "IBIT")
current_year <- as.integer(format(Sys.Date(), "%Y"))
years <- (current_year - 3):current_year

# Optional if you have not synced locally yet:
# invisible(lapply(tickers, investdatar::sync_local_ishare_data))
# investdatar::sync_local_fred_data("DGS10")

build_bh_bt_res_list <- function(year_data) {
  ret_list <- lapply(names(year_data), function(ticker) {
    dt <- copy(year_data[[ticker]])
    dt[, log_ret := log1p(simple_ret)]
    dt <- dt[, .(date, log_ret)]
    setnames(dt, "log_ret", ticker)
    dt
  })
  names(ret_list) <- names(year_data)

  aligned_dt <- Reduce(function(x, y) merge(x, y, by = "date", all = FALSE), ret_list)
  if (nrow(aligned_dt) < 20L) {
    stop("Not enough overlapping return observations across ETFs for optimization.", call. = FALSE)
  }

  data.table::rbindlist(lapply(names(year_data), function(ticker) {
    log_ret <- aligned_dt[[ticker]]
    annual_return <- exp(mean(log_ret, na.rm = TRUE) * 252) - 1
    data.table(
      asset_name = ticker,
      strat_label = "buy_and_hold",
      annual_return = annual_return,
      log_ret_dt = list(data.table(
        datetime = as.POSIXct(aligned_dt$date, tz = "UTC"),
        log_ret = log_ret
      ))
    )
  }), fill = TRUE)
}

format_weight_caption <- function(weights, tickers, top_n = 4L) {
  wt <- data.table(
    ticker = tickers,
    weight = as.numeric(weights)
  )[abs(weight) > 1e-6]
  if (nrow(wt) == 0L) return("Tangency mix: no feasible allocation")
  wt <- wt[order(-weight)][1:min(top_n, .N)]
  paste0(
    "Tangency mix: ",
    paste(sprintf("%s %.0f%%", wt$ticker, wt$weight * 100), collapse = "; ")
  )
}

safe_get_optimal_weights <- function(bt_res_list, target_return) {
  tryCatch(
    investlabr::get_optimal_weights(bt_res_list, target_return = target_return),
    error = function(e) NULL
  )
}

make_year_panel <- function(year_t, hist_list, dgs10_dt) {
  year_data <- lapply(names(hist_list), function(ticker) {
    dt <- copy(hist_list[[ticker]])
    dt <- dt[as.integer(format(date, "%Y")) == year_t & !is.na(nav)]
    dt <- unique(dt, by = "date")
    dt[, nav := as.numeric(nav)]
    dt <- dt[!is.na(nav)]
    if (nrow(dt) < 40L) return(NULL)
    dt[, simple_ret := nav / shift(nav) - 1]
    dt <- dt[!is.na(simple_ret)]
    if (nrow(dt) < 30L) return(NULL)
    dt[, ticker := ticker]
    dt
  })
  names(year_data) <- names(hist_list)
  year_data <- Filter(Negate(is.null), year_data)
  if (length(year_data) < 2L) {
    stop(sprintf("Not enough ETF history available for %s.", year_t), call. = FALSE)
  }

  bt_res_list <- build_bh_bt_res_list(year_data)

  scatter_dt <- data.table::rbindlist(lapply(year_data, function(dt) {
    data.table(
      ticker = dt$ticker[1],
      risk = stats::sd(dt$simple_ret, na.rm = TRUE),
      avg_ret = mean(dt$simple_ret, na.rm = TRUE)
    )
  }))

  ret_mat <- do.call(cbind, lapply(bt_res_list$log_ret_dt, function(x) x$log_ret))
  colnames(ret_mat) <- bt_res_list$asset_name
  cov_ann <- stats::cov(ret_mat, use = "pairwise.complete.obs") * 252
  mu_daily <- scatter_dt[match(bt_res_list$asset_name, ticker), avg_ret]

  mu_ann_min <- min(bt_res_list$annual_return, na.rm = TRUE)
  mu_ann_max <- max(bt_res_list$annual_return, na.rm = TRUE)
  target_levels <- seq(mu_ann_min + 1e-6, mu_ann_max - 1e-6, length.out = 15L)

  frontier_rows <- lapply(target_levels, function(target_return) {
    weight_res <- safe_get_optimal_weights(bt_res_list, target_return = target_return)
    if (is.null(weight_res)) return(NULL)
    weights <- weight_res$optimal_weights
    if (is.null(weights) || length(weights) == 0L) return(NULL)
    if (length(weights) != nrow(scatter_dt)) return(NULL)
    weights <- as.numeric(weights)

    risk_val <- sqrt(drop(t(weights) %*% cov_ann %*% weights)) / sqrt(252)
    avg_ret_val <- sum(weights * mu_daily)
    if (!is.finite(risk_val) || !is.finite(avg_ret_val) || risk_val <= 0) return(NULL)

    data.table(
      target_return = target_return,
      risk = risk_val,
      avg_ret = avg_ret_val,
      weights = I(list(weights))
    )
  })

  frontier_rows <- Filter(Negate(is.null), frontier_rows)
  if (length(frontier_rows) < 2L) {
    # Fallback: probe a denser target grid before declaring failure.
    dense_targets <- seq(mu_ann_min + 1e-6, mu_ann_max - 1e-6, length.out = 50L)
    frontier_rows <- lapply(dense_targets, function(target_return) {
      weight_res <- safe_get_optimal_weights(bt_res_list, target_return = target_return)
      if (is.null(weight_res)) return(NULL)
      weights <- weight_res$optimal_weights
      if (is.null(weights) || length(weights) == 0L) return(NULL)
      if (length(weights) != nrow(scatter_dt)) return(NULL)
      weights <- as.numeric(weights)

      risk_val <- sqrt(drop(t(weights) %*% cov_ann %*% weights)) / sqrt(252)
      avg_ret_val <- sum(weights * mu_daily)
      if (!is.finite(risk_val) || !is.finite(avg_ret_val) || risk_val <= 0) return(NULL)

      data.table(
        target_return = target_return,
        risk = risk_val,
        avg_ret = avg_ret_val,
        weights = I(list(weights))
      )
    })
    frontier_rows <- Filter(Negate(is.null), frontier_rows)
  }

  if (length(frontier_rows) == 0L) {
    included <- paste(bt_res_list$asset_name, collapse = ", ")
    stop(
      sprintf(
        "No feasible efficient-frontier points found for %s. Included ETFs: %s. Annual return range: [%.4f, %.4f].",
        year_t, included, mu_ann_min, mu_ann_max
      ),
      call. = FALSE
    )
  }

  frontier_dt <- data.table::rbindlist(frontier_rows, fill = TRUE)

  frontier_dt <- frontier_dt[order(risk)]

  dgs10_year <- copy(dgs10_dt)[as.integer(format(date, "%Y")) == year_t & !is.na(value)]
  rf_daily <- if (nrow(dgs10_year) == 0L) 0 else mean(dgs10_year$value, na.rm = TRUE) / 100 / 252

  frontier_dt[, sharpe_like := (avg_ret - rf_daily) / risk]
  tangency_idx <- frontier_dt[is.finite(sharpe_like) & !is.na(sharpe_like), which.max(sharpe_like)]
  if (length(tangency_idx) == 0L || is.na(tangency_idx)) {
    stop(sprintf("No valid tangency point found for %s.", year_t), call. = FALSE)
  }
  tangency_dt <- frontier_dt[tangency_idx]
  tangent_slope <- (tangency_dt$avg_ret - rf_daily) / tangency_dt$risk
  tangent_x <- c(0, max(frontier_dt$risk, scatter_dt$risk, na.rm = TRUE) * 1.05)
  tangent_dt <- data.table(
    risk = tangent_x,
    avg_ret = rf_daily + tangent_slope * tangent_x
  )

  tangent_caption <- format_weight_caption(
    weights = tangency_dt$weights[[1]],
    tickers = bt_res_list$asset_name
  )
  tangent_caption <- paste(
    tangent_caption,
    sprintf(
      "Tangency point: ann. return %.2f%% | ann. risk %.2f%%",
      tangency_dt$avg_ret * 252 * 100,
      tangency_dt$risk * sqrt(252) * 100
    ),
    sep = "\n"
  )

  palette_vals <- rep_len(
    investlabr::viz_palette_get("institutional_blue", "report", "discrete"),
    nrow(scatter_dt)
  )

  p <- ggplot() +
    geom_point(
      data = scatter_dt,
      aes(x = risk, y = avg_ret, color = ticker),
      size = 2.4
    ) +
    geom_text(
      data = scatter_dt,
      aes(x = risk, y = avg_ret, label = ticker, color = ticker),
      nudge_y = 0.00005,
      show.legend = FALSE,
      size = 3.2
    ) +
    geom_line(
      data = frontier_dt,
      aes(x = risk, y = avg_ret),
      color = investlabr::viz_style_get("institutional_blue", "report")$accent,
      linewidth = 0.9
    ) +
    geom_line(
      data = tangent_dt,
      aes(x = risk, y = avg_ret),
      linetype = "dashed",
      color = investlabr::viz_style_get("institutional_blue", "report")$accent2,
      linewidth = 0.7
    ) +
    geom_point(
      data = data.table(risk = 0, avg_ret = rf_daily),
      aes(x = risk, y = avg_ret),
      color = investlabr::viz_style_get("institutional_blue", "report")$muted,
      size = 2
    ) +
    geom_point(
      data = tangency_dt,
      aes(x = risk, y = avg_ret),
      color = investlabr::viz_style_get("institutional_blue", "report")$accent2,
      size = 2.2
    ) +
    labs(
      title = as.character(year_t),
      subtitle = sprintf(
        "Avg DGS10 rf: %.2f%% annualized | %.4f daily",
        rf_daily * 252 * 100,
        rf_daily
      ),
      x = "Risk (sd of daily returns)",
      y = "Average daily return",
      caption = tangent_caption
    ) +
    scale_color_manual(values = palette_vals)

  investlabr::viz_theme_apply(
    p,
    style = "institutional_blue",
    context = "report",
    legend_position = "none",
    show_compiler = FALSE
  )
}

hist_list <- lapply(tickers, function(ticker) {
  dt <- investdatar::get_local_ishare_data(ticker)
  dt <- data.table::as.data.table(dt)[, .(date, nav)]
  dt[, date := as.Date(date)]
  dt
})
names(hist_list) <- tickers

dgs10_dt <- data.table::as.data.table(investdatar::get_local_FRED_data("DGS10"))[, .(date = as.Date(date), value = as.numeric(value))]

year_plots <- lapply(years, make_year_panel, hist_list = hist_list, dgs10_dt = dgs10_dt)

investlabr::gen_grid_of_plots_with_labels(
  plots = year_plots,
  n_rows = 2,
  n_cols = 2,
  title = "Buy-and-Hold Portfolio Mix Across Rolling Calendar Years",
  bottom = "Universe: IVV, IEFA, AGG, IEMG, IAU, SLV, IBIT. Frontier uses 15 target return levels from investlabr::get_optimal_weights().",
  style = "institutional_blue",
  context = "report"
)
