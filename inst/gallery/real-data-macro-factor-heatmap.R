# Macro factor heatmap gallery
#
# What changed:
# - Broad equity controls stay in every regression but are reported separately
#   from the main macro heatmap, so S&P 500 / Nasdaq beta does not dominate the
#   research story.
# - The main panel now focuses on rates, curve, liquidity, stress, reserve,
#   credit, crypto, and semiconductor-relative exposures.
# - Optional extra factors are loaded defensively. In the current local cache,
#   the intended additions are available: BTC return, SOXX minus SPY, 10y-2y
#   nominal yield spread change, and high-yield credit-spread change. If any
#   local source is unavailable, it is skipped and documented in the final
#   caption.
# - Low-frequency FRED series such as reserves and STLFSI are treated as
#   stepped / release-based daily series after last-observation-carried-forward
#   level alignment; their changes should be read as release-date impulses, not
#   true continuously observed daily information.

library(data.table)
library(ggplot2)
library(investdatar)
library(investlabr)

# -------------------------------------------------------------------------
# Configuration
# -------------------------------------------------------------------------

tickers <- c(
  "AAPL", "GOOG", "BMNR", "MSTR", "COIN", "TSLA", "PLTR", "ORCL",
  "NVDA", "AVGO", "ASML", "AMZN", "MSFT", "AMD", "IBM"
)

start_date <- as.Date("2015-01-01")
end_date <- Sys.Date()
min_obs_full <- 252L
alignment <- "daily" # Use "weekly" for a lower-frequency robustness view.
show_macro_values <- TRUE

market_factor_terms <- c("mkt_sp500_ret", "nasdaq_rel_ret")

optional_factor_sources <- c(
  "BTC-USD", "SOXX", "SPY", "DGS2", "DGS10", "DFII10",
  "BAMLH0A0HYM2", "SOFR", "IORB", "VIXCLS", "STLFSI2", "WRBWFRBL"
)

# Optional if you have not synced locally yet:
# invisible(lapply(
#   unique(c(tickers, "BTC-USD", "SOXX", "SPY")),
#   function(tk) investdatar::sync_local_quantmod_OHLC(
#     ticker = tk, label = tk, from = start_date, to = end_date, src = "yahoo"
#   )
# ))
# invisible(lapply(
#   c("SP500", "NASDAQCOM", "DGS2", "DGS10", "DFII10",
#     "BAMLH0A0HYM2", "SOFR", "IORB", "VIXCLS", "STLFSI2", "WRBWFRBL"),
#   investdatar::sync_local_fred_data
# ))

# -------------------------------------------------------------------------
# Utility helpers
# -------------------------------------------------------------------------

zscore <- function(x) {
  mu <- mean(x, na.rm = TRUE)
  sdv <- stats::sd(x, na.rm = TRUE)
  if (!is.finite(sdv) || sdv == 0) return(rep(NA_real_, length(x)))
  (x - mu) / sdv
}

safe_first_col <- function(nms, candidates) {
  hit <- candidates[candidates %in% nms]
  if (length(hit) == 0L) return(NA_character_)
  hit[[1L]]
}

make_calendar <- function() {
  data.table(date = seq(start_date, end_date, by = "day"))
}

align_level_to_calendar <- function(dt, value_col = "value") {
  if (is.null(dt) || nrow(dt) == 0L || !value_col %in% names(dt)) return(NULL)
  calendar_dt <- make_calendar()
  out <- merge(
    calendar_dt,
    dt[, .(date = as.Date(date), value = as.numeric(get(value_col)))],
    by = "date",
    all.x = TRUE
  )
  data.table::setorder(out, date)
  out[, value := data.table::nafill(value, type = "locf")]
  out
}

make_change_factor <- function(dt, term, log_change = FALSE) {
  out <- align_level_to_calendar(dt)
  if (is.null(out)) return(NULL)
  if (isTRUE(log_change)) {
    out[, value := c(NA_real_, diff(log(value)))]
  } else {
    out[, value := c(NA_real_, diff(value))]
  }
  out[, .(date, value, term = term)]
}

make_return_factor <- function(dt, term) {
  out <- align_level_to_calendar(dt, value_col = "price")
  if (is.null(out)) return(NULL)
  out[, value := c(NA_real_, diff(log(value)))]
  out[, .(date, value, term = term)]
}

safe_merge_factor <- function(registry, factor_dt, term, label, group, note, skipped) {
  if (is.null(factor_dt) || nrow(factor_dt) == 0L || !"value" %in% names(factor_dt)) {
    skipped[[length(skipped) + 1L]] <- paste0(label, " (", note, ")")
    return(list(registry = registry, skipped = skipped))
  }

  usable <- factor_dt[is.finite(value)]
  if (nrow(usable) < min_obs_full) {
    skipped[[length(skipped) + 1L]] <- paste0(label, " (insufficient aligned observations)")
    return(list(registry = registry, skipped = skipped))
  }

  registry[[length(registry) + 1L]] <- list(
    term = term,
    label = label,
    group = group,
    data = factor_dt[, .(date, value)]
  )
  list(registry = registry, skipped = skipped)
}

to_weekly_sum <- function(dt, value_cols) {
  out <- copy(dt)
  out[, week := as.Date(cut(date, breaks = "week", start.on.monday = TRUE))]
  out <- out[, lapply(.SD, function(x) sum(x, na.rm = TRUE)), by = week, .SDcols = value_cols]
  data.table::setnames(out, "week", "date")
  out
}

# -------------------------------------------------------------------------
# Price and factor loaders
# -------------------------------------------------------------------------

get_stock_price_dt <- function(ticker) {
  dt <- tryCatch(
    data.table::as.data.table(investdatar::get_local_quantmod_OHLC(ticker, src = "yahoo")),
    error = function(e) NULL
  )

  if (!is.null(dt) && nrow(dt) > 0L) {
    price_col <- safe_first_col(names(dt), c("adj_close", "adjusted", "close"))
    if (!is.na(price_col)) {
      out <- dt[, .(date = as.Date(date), price = as.numeric(get(price_col)))]
      out <- out[date >= start_date & date <= end_date & is.finite(price)]
      if (nrow(out) > 0L) return(out)
    }
  }

  legacy_path <- tryCatch(
    file.path(investdatar::get_source_data_path("yahoofinance", create = FALSE), paste0(ticker, ".rds")),
    error = function(e) NA_character_
  )
  if (is.na(legacy_path) || !file.exists(legacy_path)) return(NULL)

  legacy_dt <- tryCatch(data.table::as.data.table(readRDS(legacy_path)), error = function(e) NULL)
  if (is.null(legacy_dt) || nrow(legacy_dt) == 0L) return(NULL)

  date_col <- safe_first_col(tolower(names(legacy_dt)), "date")
  close_col <- safe_first_col(tolower(names(legacy_dt)), c("adj_close", "adjusted", "close"))
  if (is.na(date_col) || is.na(close_col)) return(NULL)

  original_date_col <- names(legacy_dt)[tolower(names(legacy_dt)) == date_col][1L]
  original_close_col <- names(legacy_dt)[tolower(names(legacy_dt)) == close_col][1L]
  out <- legacy_dt[, .(
    date = as.Date(get(original_date_col)),
    price = as.numeric(get(original_close_col))
  )]
  out[date >= start_date & date <= end_date & is.finite(price)]
}

get_fred_dt <- function(series_id) {
  dt <- tryCatch(
    data.table::as.data.table(investdatar::get_local_FRED_data(series_id)),
    error = function(e) NULL
  )
  if (is.null(dt) || nrow(dt) == 0L || !"value" %in% names(dt)) return(NULL)
  out <- dt[date >= start_date & date <= end_date & !is.na(value), .(
    date = as.Date(date),
    value = as.numeric(value)
  )]
  data.table::setorder(out, date)
  if (nrow(out) == 0L) return(NULL)
  out
}

# -------------------------------------------------------------------------
# Factor engineering
# -------------------------------------------------------------------------

build_factor_registry <- function() {
  registry <- list()
  skipped <- list()

  sp500_dt <- get_fred_dt("SP500")
  nasdaq_dt <- get_fred_dt("NASDAQCOM")
  if (is.null(sp500_dt) || is.null(nasdaq_dt)) {
    stop("Required market controls SP500 and NASDAQCOM are unavailable in local FRED cache.", call. = FALSE)
  }

  sp500_ret <- make_change_factor(sp500_dt, "mkt_sp500_ret", log_change = TRUE)
  nasdaq_ret <- make_change_factor(nasdaq_dt, "nasdaq_ret", log_change = TRUE)
  market_dt <- merge(
    sp500_ret[, .(date, mkt_sp500_ret = value)],
    nasdaq_ret[, .(date, nasdaq_ret = value)],
    by = "date",
    all = TRUE
  )
  market_dt[, nasdaq_rel_ret := nasdaq_ret - mkt_sp500_ret]

  registry[[length(registry) + 1L]] <- list(
    term = "mkt_sp500_ret",
    label = "S&P 500 return",
    group = "market",
    data = market_dt[, .(date, value = mkt_sp500_ret)]
  )
  registry[[length(registry) + 1L]] <- list(
    term = "nasdaq_rel_ret",
    label = "Nasdaq minus\nS&P 500",
    group = "market",
    data = market_dt[, .(date, value = nasdaq_rel_ret)]
  )

  add_factor <- function(factor_dt, term, label, group, note) {
    res <- safe_merge_factor(registry, factor_dt, term, label, group, note, skipped)
    registry <<- res$registry
    skipped <<- res$skipped
  }

  btc_dt <- get_stock_price_dt("BTC-USD")
  add_factor(make_return_factor(btc_dt, "btc_ret"), "btc_ret", "BTC return", "macro", "BTC-USD local Yahoo data unavailable")

  soxx_dt <- get_stock_price_dt("SOXX")
  spy_dt <- get_stock_price_dt("SPY")
  if (!is.null(soxx_dt) && !is.null(spy_dt)) {
    soxx_ret <- make_return_factor(soxx_dt, "soxx_ret")
    spy_ret <- make_return_factor(spy_dt, "spy_ret")
    semi_dt <- merge(soxx_ret[, .(date, soxx_ret = value)], spy_ret[, .(date, spy_ret = value)], by = "date", all = TRUE)
    semi_dt[, value := soxx_ret - spy_ret]
    add_factor(semi_dt[, .(date, value)], "semi_rel_ret", "Semis minus\nSPY", "macro", "SOXX/SPY local Yahoo data unavailable")
  } else {
    skipped[[length(skipped) + 1L]] <- "Semis minus SPY (SOXX or SPY local Yahoo data unavailable)"
  }

  dgs2_dt <- get_fred_dt("DGS2")
  dgs10_dt <- get_fred_dt("DGS10")

  if (!is.null(dgs2_dt) && !is.null(dgs10_dt)) {
    dgs2_level <- align_level_to_calendar(dgs2_dt)
    dgs10_level <- align_level_to_calendar(dgs10_dt)
    curve_dt <- merge(dgs2_level[, .(date, dgs2 = value)], dgs10_level[, .(date, dgs10 = value)], by = "date", all = TRUE)
    curve_dt[, value := c(NA_real_, diff(dgs10 - dgs2))]
    add_factor(curve_dt[, .(date, value)], "curve_2s10s_chg", "10y-2y nominal\nspread change", "macro", "DGS2/DGS10 unavailable")
  } else {
    skipped[[length(skipped) + 1L]] <- "10y-2y nominal spread change (DGS2 or DGS10 unavailable)"
  }

  add_factor(make_change_factor(get_fred_dt("DFII10"), "real10y_chg"), "real10y_chg", "10y real yield\nchange", "macro", "DFII10 unavailable")

  sofr_dt <- get_fred_dt("SOFR")
  iorb_dt <- get_fred_dt("IORB")
  if (!is.null(sofr_dt) && !is.null(iorb_dt)) {
    sofr_level <- align_level_to_calendar(sofr_dt)
    iorb_level <- align_level_to_calendar(iorb_dt)
    liq_dt <- merge(sofr_level[, .(date, sofr = value)], iorb_level[, .(date, iorb = value)], by = "date", all = TRUE)
    liq_dt[, value := c(NA_real_, diff(sofr - iorb))]
    add_factor(liq_dt[, .(date, value)], "liq_sofr_iorb_chg", "SOFR-IORB\nchange", "macro", "SOFR/IORB unavailable")
  } else {
    skipped[[length(skipped) + 1L]] <- "SOFR-IORB change (SOFR or IORB unavailable)"
  }

  add_factor(make_change_factor(get_fred_dt("BAMLH0A0HYM2"), "credit_hy_chg"), "credit_hy_chg", "HY credit spread\nchange", "macro", "BAMLH0A0HYM2 unavailable")
  add_factor(make_change_factor(get_fred_dt("VIXCLS"), "vix_chg"), "vix_chg", "VIX change", "macro", "VIXCLS unavailable")
  add_factor(make_change_factor(get_fred_dt("STLFSI2"), "stlfsi_chg"), "stlfsi_chg", "Financial stress\nchange", "macro", "STLFSI2 unavailable")
  add_factor(make_change_factor(get_fred_dt("WRBWFRBL"), "reserve_log_chg", log_change = TRUE), "reserve_log_chg", "Reserve balances\ngrowth", "macro", "WRBWFRBL unavailable")

  list(registry = registry, skipped = unlist(skipped, use.names = FALSE))
}

registry_res <- build_factor_registry()
factor_registry <- registry_res$registry
skipped_optional_factors <- registry_res$skipped

factor_meta <- rbindlist(lapply(factor_registry, function(x) {
  data.table(term = x$term, factor_label = x$label, factor_group = x$group)
}), fill = TRUE)

factor_dt <- Reduce(function(x, y) merge(x, y, by = "date", all = TRUE), lapply(factor_registry, function(x) {
  out <- copy(x$data)
  data.table::setnames(out, "value", x$term)
  out
}))
data.table::setorder(factor_dt, date)
factor_cols <- setdiff(names(factor_dt), "date")

if (identical(alignment, "weekly")) {
  factor_dt <- to_weekly_sum(factor_dt, factor_cols)
}

factor_dt <- factor_dt[complete.cases(factor_dt)]
factor_dt[, (factor_cols) := lapply(.SD, zscore), .SDcols = factor_cols]

market_factor_terms <- intersect(market_factor_terms, factor_cols)
macro_factor_terms <- factor_meta[factor_group == "macro" & term %in% factor_cols, term]

if (length(market_factor_terms) < 2L) {
  stop("Both market control factors are required for this gallery example.", call. = FALSE)
}
if (length(macro_factor_terms) == 0L) {
  stop("No macro factors were available after local-cache loading and alignment.", call. = FALSE)
}

# -------------------------------------------------------------------------
# Regression estimation
# -------------------------------------------------------------------------

estimate_one_ticker <- function(ticker) {
  px <- get_stock_price_dt(ticker)
  if (is.null(px) || nrow(px) < (min_obs_full + 20L)) return(NULL)

  data.table::setorder(px, date)
  px[, stock_ret := c(NA_real_, diff(log(price)))]
  px <- px[!is.na(stock_ret), .(date, stock_ret)]
  if (identical(alignment, "weekly")) {
    px <- to_weekly_sum(px, "stock_ret")
  }
  px[, stock_ret := zscore(stock_ret)]

  aligned_dt <- merge(px, factor_dt, by = "date", all = FALSE)
  aligned_dt <- aligned_dt[complete.cases(aligned_dt)]
  if (nrow(aligned_dt) < min_obs_full) return(NULL)

  model_terms <- c(market_factor_terms, macro_factor_terms)
  fit <- stats::lm(
    stats::as.formula(paste("stock_ret ~", paste(model_terms, collapse = " + "))),
    data = aligned_dt
  )
  coef_dt <- data.table::as.data.table(summary(fit)$coefficients, keep.rownames = "term")
  data.table::setnames(coef_dt, c("term", "estimate", "std_error", "t_value", "p_value"))
  coef_dt <- coef_dt[term != "(Intercept)" & term %in% model_terms]
  coef_dt[, `:=`(
    ticker = ticker,
    n_obs = nrow(aligned_dt)
  )]
  coef_dt
}

beta_dt_full <- data.table::rbindlist(lapply(tickers, estimate_one_ticker), fill = TRUE)
if (nrow(beta_dt_full) == 0L) {
  stop("No locally available stock series produced enough observations for the heatmap. Sync the requested Yahoo tickers first.", call. = FALSE)
}

beta_dt_full <- merge(beta_dt_full, factor_meta, by = "term", all.x = TRUE)
beta_dt_full[, star := data.table::fifelse(
  p_value < 0.01,
  "**",
  data.table::fifelse(p_value < 0.05, "*", "")
)]

beta_dt_market <- beta_dt_full[term %in% market_factor_terms]
beta_dt_macro <- beta_dt_full[term %in% macro_factor_terms]

# -------------------------------------------------------------------------
# Ticker ordering
# -------------------------------------------------------------------------

order_tickers <- function(beta_macro) {
  wide <- data.table::dcast(beta_macro, ticker ~ term, value.var = "estimate")
  ticker_vec <- wide$ticker
  mat <- as.matrix(wide[, setdiff(names(wide), "ticker"), with = FALSE])
  rownames(mat) <- ticker_vec
  mat[!is.finite(mat)] <- 0

  if (nrow(mat) >= 2L && ncol(mat) >= 2L) {
    return(rownames(mat)[stats::hclust(stats::dist(mat))$order])
  }

  if ("btc_ret" %in% names(wide)) {
    return(wide[order(-btc_ret), ticker])
  }
  if ("semi_rel_ret" %in% names(wide)) {
    return(wide[order(-semi_rel_ret), ticker])
  }
  sort(ticker_vec)
}

ticker_order <- order_tickers(beta_dt_macro)
beta_dt_full[, ticker := factor(ticker, levels = ticker_order)]
beta_dt_market[, ticker := factor(ticker, levels = ticker_order)]
beta_dt_macro[, ticker := factor(ticker, levels = ticker_order)]

market_label_levels <- factor_meta[term %in% market_factor_terms, factor_label]
macro_label_levels <- factor_meta[term %in% macro_factor_terms, factor_label]
beta_dt_market[, factor_label := factor(factor_label, levels = market_label_levels)]
beta_dt_macro[, factor_label := factor(factor_label, levels = macro_label_levels)]

# -------------------------------------------------------------------------
# Plotting
# -------------------------------------------------------------------------

fill_limit <- max(abs(beta_dt_full$estimate), na.rm = TRUE)
if (!is.finite(fill_limit) || fill_limit <= 0) fill_limit <- 1

make_heatmap_market <- function(dt) {
  p <- ggplot(dt, aes(x = factor_label, y = ticker, fill = estimate)) +
    geom_tile(color = "white", linewidth = 0.3) +
    geom_text(aes(label = star), size = 4.0, color = "black") +
    scale_fill_gradient2(
      low = "#B35C44",
      mid = "#F8F8F4",
      high = "#355C7D",
      midpoint = 0,
      limits = c(-fill_limit, fill_limit),
      oob = scales::squish
    ) +
    labs(
      title = "Market controls",
      subtitle = "Included in the model, reported separately.",
      x = NULL,
      y = NULL,
      fill = "Beta"
    )

  investlabr::viz_theme_apply(
    p,
    style = "institutional_blue",
    context = "report",
    legend_position = "none",
    show_compiler = FALSE
  )
}

make_heatmap_macro <- function(dt) {
  label_col <- if (isTRUE(show_macro_values) && uniqueN(dt$term) <= 12L && uniqueN(dt$ticker) <= 18L) {
    sprintf("%.2f%s", dt$estimate, dt$star)
  } else {
    dt$star
  }
  plot_dt <- copy(dt)
  plot_dt[, tile_label := label_col]

  p <- ggplot(plot_dt, aes(x = factor_label, y = ticker, fill = estimate)) +
    geom_tile(color = "white", linewidth = 0.3) +
    geom_text(aes(label = tile_label), size = 2.45, color = "black") +
    scale_fill_gradient2(
      low = "#B35C44",
      mid = "#F8F8F4",
      high = "#355C7D",
      midpoint = 0,
      limits = c(-fill_limit, fill_limit),
      oob = scales::squish
    ) +
    labs(
      title = "Macro, rates, liquidity, risk, and thematic exposures",
      subtitle = "Ticker order uses hierarchical clustering on displayed macro betas.",
      x = NULL,
      y = NULL,
      fill = "Std. beta"
    ) +
    theme(axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 1))

  investlabr::viz_theme_apply(
    p,
    style = "institutional_blue",
    context = "report",
    legend_position = "right",
    show_compiler = FALSE
  )
}

p_market <- make_heatmap_market(beta_dt_market)
p_macro <- make_heatmap_macro(beta_dt_macro)

included_optional <- setdiff(factor_meta[factor_group == "macro", factor_label], character())
included_optional_text <- paste(gsub("\n", " ", included_optional), collapse = ", ")
if (length(skipped_optional_factors) == 0L) {
  skipped_optional_text <- "none"
} else {
  skipped_optional_text <- paste(skipped_optional_factors, collapse = "; ")
}

included_tickers <- beta_dt_full[, sort(unique(as.character(ticker)))]
avg_obs <- round(mean(unique(beta_dt_full[, .(ticker, n_obs)])$n_obs))

caption_text <- paste(
  paste0(
    "Standardized full-sample regressions from ", format(start_date, "%Y-%m-%d"),
    " to ", format(end_date, "%Y-%m-%d"), "; alignment = ", alignment,
    "; * p < 0.05, ** p < 0.01."
  ),
  paste0("Market controls are included in every regression but separated from the main macro heatmap: ",
         paste(factor_meta[term %in% market_factor_terms, gsub("\n", " ", factor_label)], collapse = ", "), "."),
  paste0("Displayed macro factors included: ", included_optional_text, "."),
  paste0("Skipped optional factors: ", skipped_optional_text, "."),
  paste0("Included stocks: ", paste(included_tickers, collapse = ", "), ". Average aligned observations per stock: ", avg_obs, ".")
)
caption_text <- investlabr:::.investlabr_compiler_caption(caption_text)

title_grob <- grid::textGrob(
  "Macro Factor Sensitivities for AI, Tech, and Crypto-Linked Equities",
  gp = grid::gpar(fontsize = 16, fontface = "bold", col = investlabr::viz_style_get("institutional_blue", "report")$ink)
)
subtitle_grob <- grid::textGrob(
  "Market beta remains a model control; the main heatmap emphasizes non-market macro and thematic exposures.",
  gp = grid::gpar(fontsize = 10.5, col = investlabr::viz_style_get("institutional_blue", "report")$muted)
)
top_grob <- gridExtra::arrangeGrob(title_grob, subtitle_grob, ncol = 1, heights = grid::unit.c(grid::unit(0.55, "cm"), grid::unit(0.45, "cm")))

bottom_grob <- grid::textGrob(
  paste(strwrap(caption_text, width = 160), collapse = "\n"),
  x = grid::unit(0, "npc"),
  hjust = 0,
  gp = grid::gpar(fontsize = 8.5, col = investlabr::viz_style_get("institutional_blue", "report")$muted)
)

final_board <- gridExtra::arrangeGrob(
  p_market,
  p_macro,
  ncol = 2,
  widths = grid::unit.c(grid::unit(0.42, "null"), grid::unit(1, "null")),
  top = top_grob,
  bottom = bottom_grob
)

# -------------------------------------------------------------------------
# Output
# -------------------------------------------------------------------------

if (interactive()) {
  grid::grid.newpage()
  grid::grid.draw(final_board)
}
