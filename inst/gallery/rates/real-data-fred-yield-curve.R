library(investdatar)
library(investlabr)

series_ids <- c(
  "DGS1MO", "DGS3MO", "DGS6MO", "DGS1", "DGS2", "DGS3",
  "DGS5", "DGS7", "DGS10", "DGS20", "DGS30"
)

invisible(lapply(series_ids, investdatar::sync_local_fred_data))

yield_dt_list <- lapply(series_ids, investdatar::get_local_FRED_data)

curve_dt <- investlabr::get_yield_data_DT(
  yield_dt_list = yield_dt_list,
  yield_dates = list(
    Now = Sys.Date(),
    `One month ago` = Sys.Date() - 30,
    `One year ago` = Sys.Date() - 365
  )
)

investlabr::gen_yield_curve_plot(
  curve_dt,
  selected_windows = c("Now", "One month ago", "One year ago"),
  style = "macro_classic",
  context = "report"
)
