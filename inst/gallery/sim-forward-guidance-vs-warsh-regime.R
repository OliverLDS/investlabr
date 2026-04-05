library(data.table)
library(ggplot2)
library(investlabr)

set.seed(123)

n <- 1200L
dates <- as.Date("2015-01-01") + 0:(n - 1L)
t_break <- 600L
drift_1 <- 0.0005
drift_2 <- -0.0003
sigma_f <- 0.01
y0 <- 2

sigma_nfg <- 0.05
adapt_slow <- 0.02
sigma_fg <- 0.01
lag_days <- 80L
t_pivot <- t_break + lag_days
adapt_fast <- 0.15

roll_mean <- function(x, w) {
  n <- length(x)
  res <- rep(NA_real_, n)
  if (n >= w) {
    for (i in seq(w, n)) {
      res[i] <- mean(x[(i - w + 1L):i], na.rm = TRUE)
    }
  }
  res
}

f <- numeric(n)
f[1] <- 0
for (t in 2:n) {
  drift_t <- if (t <= t_break) drift_1 else drift_2
  f[t] <- f[t - 1L] + drift_t + rnorm(1L, 0, sigma_f)
}

eps_nfg <- rnorm(n, 0, sigma_nfg)
yield_nfg <- y0 + f + eps_nfg

yield_fg <- numeric(n)
yield_fg[1] <- y0 + f[1]
for (t in 2:n) {
  target <- y0 + f[t]
  if (t < t_pivot) {
    yield_fg[t] <- yield_fg[t - 1L] +
      adapt_slow * (target - yield_fg[t - 1L]) +
      rnorm(1L, 0, sigma_fg)
  } else if (t == t_pivot) {
    catch_up_strength <- 0.9
    yield_fg[t] <- yield_fg[t - 1L] +
      catch_up_strength * (target - yield_fg[t - 1L]) +
      rnorm(1L, 0, sigma_fg)
  } else {
    yield_fg[t] <- yield_fg[t - 1L] +
      adapt_fast * (target - yield_fg[t - 1L]) +
      rnorm(1L, 0, sigma_fg)
  }
}

dy_fg <- c(NA_real_, diff(yield_fg))
dy_nfg <- c(NA_real_, diff(yield_nfg))

window <- 60L
vol_fg <- sqrt(roll_mean(dy_fg^2, window))
vol_nfg <- sqrt(roll_mean(dy_nfg^2, window))

df <- data.table(
  date = dates,
  fundamental = y0 + f,
  forward_guidance = yield_fg,
  warsh_style = yield_nfg,
  dy_forward_guidance = dy_fg,
  dy_warsh_style = dy_nfg,
  vol_forward_guidance = vol_fg,
  vol_warsh_style = vol_nfg
)

break_date <- dates[t_break]
pivot_date <- dates[t_pivot]

level_dt <- melt(
  df[, .(date, fundamental, forward_guidance, warsh_style)],
  id.vars = "date",
  variable.name = "series",
  value.name = "value"
)
level_dt[, series := factor(
  series,
  levels = c("fundamental", "forward_guidance", "warsh_style"),
  labels = c("Fundamental path", "Forward-guidance regime", "Fast repricing regime")
)]

change_dt <- melt(
  df[, .(date, dy_forward_guidance, dy_warsh_style)],
  id.vars = "date",
  variable.name = "series",
  value.name = "value"
)
change_dt[, series := factor(
  series,
  levels = c("dy_forward_guidance", "dy_warsh_style"),
  labels = c("Forward-guidance regime", "Fast repricing regime")
)]

vol_dt <- melt(
  df[, .(date, vol_forward_guidance, vol_warsh_style)],
  id.vars = "date",
  variable.name = "series",
  value.name = "value"
)
vol_dt[, series := factor(
  series,
  levels = c("vol_forward_guidance", "vol_warsh_style"),
  labels = c("Forward-guidance regime", "Fast repricing regime")
)]

level_colors <- c(
  "Fundamental path" = "#4F6D7A",
  "Forward-guidance regime" = "#B35C44",
  "Fast repricing regime" = "#355C7D"
)
two_line_colors <- c(
  "Forward-guidance regime" = "#B35C44",
  "Fast repricing regime" = "#355C7D"
)

make_panel <- function(dt, x, y, title, subtitle = NULL, y_lab = NULL, colors, show_compiler = FALSE) {
  p <- ggplot(dt, aes(x = .data[[x]], y = .data[[y]], color = series)) +
    geom_line(linewidth = 0.8, na.rm = TRUE) +
    geom_vline(
      xintercept = as.POSIXct(c(break_date, pivot_date), tz = "UTC"),
      linetype = c("dashed", "dotted"),
      linewidth = c(0.5, 0.7),
      color = c("#7D7D7D", "#B35C44")
    ) +
    scale_color_manual(values = colors, drop = FALSE) +
    labs(
      title = title,
      subtitle = subtitle,
      x = "",
      y = y_lab,
      color = NULL
    )

  investlabr::viz_theme_apply(
    p,
    style = "institutional_blue",
    context = "report",
    legend_position = "bottom",
    show_compiler = show_compiler
  )
}

p_levels <- make_panel(
  dt = level_dt,
  x = "date",
  y = "value",
  title = "Levels: anchored repricing versus fast repricing",
  subtitle = "Dashed line = structural break | dotted line = delayed pivot in the forward-guidance regime",
  y_lab = "Level (%)",
  colors = level_colors
)

p_changes <- make_panel(
  dt = change_dt,
  x = "date",
  y = "value",
  title = "Daily changes",
  subtitle = "The anchored regime suppresses day-to-day moves before the pivot, then catches up abruptly",
  y_lab = "Daily change",
  colors = two_line_colors
)

p_vol <- make_panel(
  dt = vol_dt,
  x = "date",
  y = "value",
  title = "Rolling volatility",
  subtitle = "60-day rolling volatility of daily changes",
  y_lab = "Rolling volatility",
  colors = two_line_colors
)

pivot_window <- seq.int(max(1L, t_pivot - 5L), min(n, t_pivot + 5L))
fg_pivot_move <- mean(abs(df$dy_forward_guidance[pivot_window]), na.rm = TRUE)
warsh_pivot_move <- mean(abs(df$dy_warsh_style[pivot_window]), na.rm = TRUE)

bottom_text <- paste0(
  "Scenario note: this is a stylized simulation rather than a historical attribution exercise. ",
  "A slower communication regime smooths the path before the break but produces a larger catch-up move near the pivot. ",
  "Mean |daily change| in the pivot window: forward-guidance regime ",
  sprintf("%.3f", fg_pivot_move),
  " versus fast repricing regime ",
  sprintf("%.3f", warsh_pivot_move),
  "."
)

investlabr::gen_grid_of_plots_with_labels(
  plots = list(p_levels, p_changes, p_vol),
  n_rows = 3,
  n_cols = 1,
  title = "Simulation: forward-guidance regime versus fast repricing regime",
  bottom = bottom_text,
  style = "institutional_blue",
  context = "report",
  show_compiler = TRUE
)
