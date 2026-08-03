library(data.table)
library(ggplot2)
library(investlabr)
library(xml2)

real_yield_url <- "https://home.treasury.gov/sites/default/files/interest-rates/real_yield.xml"
nominal_yield_url <- "https://home.treasury.gov/sites/default/files/interest-rates/yield.xml"

date_1 <- as.Date("2026-02-04")
date_2 <- as.Date("2026-02-24")

parse_treasury_date <- function(x) {
  as.Date(x, format = "%d-%b-%y")
}

safe_num <- function(node, tag) {
  el <- xml2::xml_find_first(node, paste0(".//", tag))
  if (inherits(el, "xml_missing")) return(NA_real_)
  txt <- xml2::xml_text(el)
  if (length(txt) == 0L) return(NA_real_)
  suppressWarnings(as.numeric(trimws(txt)))
}

read_real_curve <- function(url) {
  doc <- xml2::read_xml(url)
  nodes <- xml2::xml_find_all(doc, ".//G_NEW_DATE")

  wide_dt <- rbindlist(lapply(nodes, function(n) {
    date_txt <- xml2::xml_text(xml2::xml_find_first(n, ".//TIPS_CURVE_DATE"))
    data.table(
      date = parse_treasury_date(date_txt),
      `5Y` = safe_num(n, "TC_5YEAR"),
      `7Y` = safe_num(n, "TC_7YEAR"),
      `10Y` = safe_num(n, "TC_10YEAR"),
      `20Y` = safe_num(n, "TC_20YEAR"),
      `30Y` = safe_num(n, "TC_30YEAR")
    )
  }), fill = TRUE)

  long_dt <- melt(
    wide_dt[!is.na(date)],
    id.vars = "date",
    variable.name = "maturity_label",
    value.name = "yield"
  )
  long_dt[, maturity_years := c("5Y" = 5, "7Y" = 7, "10Y" = 10, "20Y" = 20, "30Y" = 30)[maturity_label]]
  long_dt[!is.na(yield)]
}

read_nominal_curve <- function(url) {
  term_def <- data.table(
    code = c(
      "BC_1MONTH", "BC_1_5MONTH", "BC_2MONTH", "BC_3MONTH", "BC_4MONTH", "BC_6MONTH",
      "BC_1YEAR", "BC_2YEAR", "BC_3YEAR", "BC_5YEAR", "BC_7YEAR", "BC_10YEAR", "BC_20YEAR", "BC_30YEAR"
    ),
    maturity_label = c("1M", "1.5M", "2M", "3M", "4M", "6M", "1Y", "2Y", "3Y", "5Y", "7Y", "10Y", "20Y", "30Y"),
    maturity_years = c(1/12, 1.5/12, 2/12, 3/12, 4/12, 6/12, 1, 2, 3, 5, 7, 10, 20, 30)
  )

  doc <- xml2::read_xml(url)
  nodes <- xml2::xml_find_all(doc, ".//G_NEW_DATE")

  wide_dt <- rbindlist(lapply(nodes, function(n) {
    date_txt <- xml2::xml_text(xml2::xml_find_first(n, ".//BID_CURVE_DATE"))
    row <- data.table(date = parse_treasury_date(date_txt))
    for (i in seq_len(nrow(term_def))) {
      row[, (term_def$maturity_label[i]) := safe_num(n, term_def$code[i])]
    }
    row
  }), fill = TRUE)

  long_dt <- melt(
    wide_dt[!is.na(date)],
    id.vars = "date",
    variable.name = "maturity_label",
    value.name = "yield"
  )
  long_dt <- merge(long_dt, term_def[, .(maturity_label, maturity_years)], by = "maturity_label", all.x = TRUE)
  long_dt[!is.na(yield)]
}

closest_or_latest <- function(dates, target_date) {
  if (target_date %in% dates) return(target_date)
  max(dates[dates <= target_date], na.rm = TRUE)
}

real_dt <- read_real_curve(real_yield_url)
nominal_dt <- read_nominal_curve(nominal_yield_url)

real_date_1 <- closest_or_latest(unique(real_dt$date), date_1)
real_date_2 <- closest_or_latest(unique(real_dt$date), date_2)
nom_date_1 <- closest_or_latest(unique(nominal_dt$date), date_1)
nom_date_2 <- closest_or_latest(unique(nominal_dt$date), date_2)

real_two <- real_dt[date %in% c(real_date_1, real_date_2)]
nominal_two <- nominal_dt[date %in% c(nom_date_1, nom_date_2)]

real_two[, curve_label := fifelse(
  date == real_date_1,
  format(real_date_1, "%Y-%m-%d"),
  format(real_date_2, "%Y-%m-%d")
)]
nominal_two[, curve_label := fifelse(
  date == nom_date_1,
  format(nom_date_1, "%Y-%m-%d"),
  format(nom_date_2, "%Y-%m-%d")
)]

breakeven_two <- merge(
  nominal_two[, .(date, maturity_label, maturity_years, nominal_yield = yield, curve_label)],
  real_two[, .(date, maturity_label, maturity_years, real_yield = yield)],
  by = c("date", "maturity_label", "maturity_years"),
  all = FALSE
)
breakeven_two[, breakeven := nominal_yield - real_yield]

nominal_change <- dcast(
  nominal_two[maturity_label %in% breakeven_two$maturity_label],
  maturity_label + maturity_years ~ curve_label,
  value.var = "yield"
)
setnames(nominal_change, c(label_1, label_2), c("nominal_1", "nominal_2"))
real_change <- dcast(
  real_two,
  maturity_label + maturity_years ~ curve_label,
  value.var = "yield"
)
setnames(real_change, c(label_1, label_2), c("real_1", "real_2"))
breakeven_change <- dcast(
  breakeven_two,
  maturity_label + maturity_years ~ curve_label,
  value.var = "breakeven"
)
setnames(breakeven_change, c(label_1, label_2), c("breakeven_1", "breakeven_2"))

label_1 <- format(real_date_1, "%Y-%m-%d")
label_2 <- format(real_date_2, "%Y-%m-%d")

change_dt <- merge(nominal_change, real_change, by = c("maturity_label", "maturity_years"))
change_dt <- merge(change_dt, breakeven_change, by = c("maturity_label", "maturity_years"))
change_dt[, `:=`(
  nominal_change = nominal_2 - nominal_1,
  real_change = real_2 - real_1,
  breakeven_change = breakeven_2 - breakeven_1
)]

change_long <- melt(
  change_dt[, .(maturity_label, maturity_years, nominal_change, real_change, breakeven_change)],
  id.vars = c("maturity_label", "maturity_years"),
  variable.name = "component",
  value.name = "change_bp"
)
change_long[, component := factor(
  component,
  levels = c("nominal_change", "real_change", "breakeven_change"),
  labels = c("Nominal", "Real", "Breakeven")
)]

p1 <- ggplot(nominal_two, aes(maturity_years, yield, color = curve_label, group = curve_label)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2) +
  scale_x_continuous(
    breaks = unique(nominal_two$maturity_years),
    labels = unique(nominal_two$maturity_label)
  ) +
  labs(
    title = "Nominal Treasury curve",
    subtitle = "Treasury XML source",
    x = "Maturity",
    y = "Yield (%)",
    color = NULL
  )
p1 <- investlabr::viz_theme_apply(
  p1,
  style = "macro_classic",
  context = "report",
  legend_position = "bottom",
  show_compiler = FALSE
)

p2 <- ggplot(real_two, aes(maturity_years, yield, color = curve_label, group = curve_label)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2) +
  scale_x_continuous(
    breaks = unique(real_two$maturity_years),
    labels = unique(real_two$maturity_label)
  ) +
  labs(
    title = "Real Treasury curve",
    subtitle = "Treasury TIPS XML source",
    x = "Maturity",
    y = "Real yield (%)",
    color = NULL
  )
p2 <- investlabr::viz_theme_apply(
  p2,
  style = "macro_classic",
  context = "report",
  legend_position = "bottom",
  show_compiler = FALSE
)

p3 <- ggplot(breakeven_two, aes(maturity_years, breakeven, color = curve_label, group = curve_label)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2) +
  scale_x_continuous(
    breaks = unique(breakeven_two$maturity_years),
    labels = unique(breakeven_two$maturity_label)
  ) +
  labs(
    title = "Implied breakeven curve",
    subtitle = "Nominal minus real yields where maturities overlap",
    x = "Maturity",
    y = "Breakeven inflation (%)",
    color = NULL
  )
p3 <- investlabr::viz_theme_apply(
  p3,
  style = "macro_classic",
  context = "report",
  legend_position = "bottom",
  show_compiler = FALSE
)

p4 <- ggplot(change_long, aes(maturity_years, change_bp, color = component, group = component)) +
  geom_hline(yintercept = 0, color = "#8C8C8C", linewidth = 0.35) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2) +
  scale_x_continuous(
    breaks = unique(change_long$maturity_years),
    labels = unique(change_long$maturity_label)
  ) +
  labs(
    title = "Curve-change decomposition",
    subtitle = paste0("Change from ", label_1, " to ", label_2),
    x = "Maturity",
    y = "Change (percentage points)",
    color = NULL
  )
p4 <- investlabr::viz_theme_apply(
  p4,
  style = "macro_classic",
  context = "report",
  legend_position = "bottom",
  show_compiler = FALSE
)

investlabr::gen_grid_of_plots_with_labels(
  plots = list(p1, p2, p3, p4),
  n_rows = 2,
  n_cols = 2,
  title = "Treasury curve decomposition board",
  bottom = paste(
    "This board uses Treasury XML source curves directly rather than point-series proxies.",
    paste0("The comparison dates are ", label_1, " and ", label_2, "."),
    "The key decomposition is nominal yields = real yields + breakeven inflation."
  ),
  style = "macro_classic",
  context = "report",
  show_compiler = TRUE
)
