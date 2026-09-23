# Fully synthetic inputs; no data service or local cache is needed.
changes <- seq(-0.10, 0.10, length.out = 40)
fan <- sim_forward_fan(4, changes, horizons = c(1, 5, 10),
                       n_paths = 100, seed = 7)
viz_forward_fan(fan, "Illustrative yield paths", "Historical-change bootstrap",
                "Yield (%)", show_compiler = FALSE)
paths <- sim_scenario_path(4, c(Easing = -0.25, Hold = 0, Tightening = 0.25),
                           horizon_months = 3, start_date = as.Date("2025-01-01"))
viz_series_lines(paths, "date", "value", color = "scenario",
                 title = "Policy scenarios", subtitle = "Explicit monthly assumptions",
                 y_label = "Rate (%)", show_compiler = FALSE)
