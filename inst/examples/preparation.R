prep_fill_forward(c(NA, 1, NA, 2))
a <- data.frame(date = as.Date("2025-01-01") + c(0, 2),
                value = c(4, 4.2), series = "A")
b <- data.frame(date = as.Date("2025-01-01") + c(1, 2),
                value = c(3, 3.1), series = "B")
prep_series_wide(list(a, b))
prep_zscore(c(1, 2, 3, 4))
prep_recent_changes(c(4, 4.1, 4.2, 4.15), lookback = 2)
score <- factor_bounded_score(prep_zscore(c(1, 2, 3, 4)))
brief_score_label(tail(score, 1))
