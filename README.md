# investlabr <img src="https://img.shields.io/badge/status-dev-blue.svg" align="right" />

## Investment Strategy Evaluation Toolkit for R

`investlabr` provides fast, modular tools for **event** and **strategy** back-testing
on trading data.  It integrates seamlessly with your
[`strategyr`](https://github.com/OliverLDS/strategyr) indicators
and [`agentr`](https://github.com/OliverLDS/agentr) R6 agents.

## Key Features

* **Event evaluation** – quantify forward returns after discrete signals
  (`eval_event_performance()`, `eval_event_plot_tsline_cum_ret()`).
* **Strategy backtesting** – convert positions to log returns and compute
  annualized performance (`eval_strat_performance()`).
* **Visualization** – equity curves, return vs. drawdown scatter plots.
* **Portfolio optimization** – minimum-variance allocation with
  `get_optimal_weights()` and `eval_portfolio_performance()`.

## Installation

```r
# From GitHub (dev version)
# install.packages("remotes")
remotes::install_github("OliverLDS/investlabr")
```

## Quick Example

```r
library(investlabr)
library(strategyr)
library(agentr)

agent <- agentr::agent$new()
DT <- agent$get_local_data_okx_candle("ETH-USDT-SWAP", "4H")

strategyr::gen_ind_EMA(DT)
strategyr::gen_ind_EMA_trend(DT)
strategyr::gen_evt_EMA_trend(DT)

# Evaluate an event
evt <- eval_event_performance(DT, "event_ema_cross_short_20_50")
eval_event_plot_tsline_cum_ret(evt)

# Evaluate a strategy
strategyr::gen_pos_EMA_trend(DT)
bt <- eval_strat_performance(DT, "pos_ema_trend_both_20_50")
eval_strat_plot_tsline_eq(bt)
```

## Requirements

* R ≥ 3.5
* Imports: `data.table`, `ggplot2`, `ggrepel`, `quadprog`, `stats`

## License

MIT © 2025 Oliver Lee
