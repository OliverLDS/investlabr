.gallery_file <- tryCatch(normalizePath(sys.frame(1)$ofile), error = function(e) NA_character_)
.gallery_dir <- if (!is.na(.gallery_file)) dirname(.gallery_file) else file.path(getwd(), "inst/gallery")
source(file.path(.gallery_dir, "macro-forecast-gallery-utils.R"))

start_date <- as.Date("1995-01-01")
# Optional sync:
# invisible(lapply(c("CPIAUCSL", "UNRATE", "WRBWFRBL"), investdatar::sync_local_fred_data))

wide <- mf_to_wide(list(mf_load_fred("CPIAUCSL", "CPI", start_date), mf_load_fred("UNRATE", "Unemployment", start_date), mf_load_fred("WRBWFRBL", "Reserves", start_date)))
wide[, inflation := 100 * (CPI / shift(CPI, 12L) - 1)]
wide[, labor_chg := Unemployment - shift(Unemployment, 6L)]
wide[, liq_chg := log(Reserves) - shift(log(Reserves), 13L)]
wide <- wide[!is.na(inflation) & !is.na(labor_chg) & !is.na(liq_chg)]
wide[, regime := fifelse(inflation > 3 & labor_chg <= 0, "Hot", fifelse(labor_chg > 0.4, "Cooling", fifelse(liq_chg < 0, "Tight liquidity", "Goldilocks")))]
wide[, next_regime := shift(regime, type = "lead")]
trans <- wide[!is.na(next_regime), .N, by = .(regime, next_regime)]
trans[, prob := N / sum(N), by = regime]
current_regime <- wide[.N, regime]
next_prob <- trans[regime == current_regime]

p1 <- ggplot(wide[date >= Sys.Date() - 365 * 5], aes(date, regime, color = regime)) + geom_point(size = 1.2) + labs(title = "Regime history", subtitle = paste("Current regime:", current_regime), x = NULL, y = NULL, color = NULL)
p1 <- mf_theme(p1)
p2 <- ggplot(trans, aes(next_regime, regime, fill = prob)) + geom_tile(color = "white") + geom_text(aes(label = sprintf("%.0f%%", 100 * prob)), size = 3) + scale_fill_gradient(low = "#F4F1EA", high = "#B24C45") + labs(title = "One-step transition matrix", subtitle = "Empirical transition probabilities from monthly-like daily labels.", x = "Next regime", y = "Current regime", fill = "Prob.")
p2 <- mf_theme(p2, "right")
p3 <- ggplot(next_prob, aes(next_regime, prob, fill = next_regime)) + geom_col(show.legend = FALSE) + scale_y_continuous(labels = scales::percent) + labs(title = "Current-state next regime probabilities", subtitle = paste("Conditioned on:", current_regime), x = NULL, y = "Probability")
p3 <- mf_theme(p3, "none")

board <- investlabr::gen_grid_of_plots_with_labels(
  list(p1, p2, p3),
  n_rows = 1,
  n_cols = 3,
  title = "Macro Regime Transition Matrix Board",
  bottom = "Regimes are intentionally simple: Hot, Cooling, Tight liquidity, and Goldilocks. Transition probabilities are empirical one-step frequencies.",
  style = mf_style,
  context = mf_context
)
print(board)
