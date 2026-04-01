#' investlabr: Research workflows for macro and cross-asset analysis
#'
#' `investlabr` provides reusable research workflows that sit on top of
#' data-access packages such as `investdatar`. The package focuses on turning
#' existing macro and market datasets into analysis-ready tables, factors,
#' event and regime views, sensitivity estimates, scenario comparisons, and
#' research-grade visuals.
#'
#' Relative to related packages:
#'
#' - `investdatar` gets and stores the data
#' - `strategyr` turns strategy logic into executable trading decisions
#' - `investlabr` helps explore, visualize, and communicate investment ideas
#'
#' `investlabr` may include lightweight and simplified backtesting helpers for
#' exploratory research and communication, but it is not intended to be the
#' package that decides live actions or produces broker-ready execution logic.
#'
#' Intended module families include:
#'
#' - `prep_*` for research data preparation
#' - `factor_*` for macro and cross-asset factor construction
#' - `event_*` and `regime_*` for time-based interpretation layers
#' - `sense_*` for sensitivity and exposure estimation
#' - `sim_*` for scenario and structural simulations
#' - `viz_*` and `brief_*` for publication-ready output
#'
#' The package does not own raw data downloading, source sync, execution,
#' portfolio accounting, or LLM orchestration.
#'
#' @name investlabr-package
#' @aliases investlabr
#' @docType package
#' @keywords internal
"_PACKAGE"
