# Gallery Promotion Backlog

Use this file as a lightweight backlog for patterns that emerge from daily-analysis gallery work. The goal is to keep scripts useful while gradually moving reusable logic into the right package.

## Near-Term Candidates

| Candidate | Current Location | Suggested Target | Reason |
| --- | --- | --- | --- |
| Local FRED loading helpers | `_shared/macro-forecast.R` and several FRED examples | `investdatar` | Repeated local-cache normalization is data-access glue and should not become an `investlabr` provider API |
| Macro monitor panel builders | `macro-monitor/real-data-fred-ci-lending-monitor.R`, policy/liquidity dashboards | `investlabr::brief_*` or `investlabr::viz_*` | Repeated multi-panel monitor pattern with local FRED cache, themed plots, and reader-facing footnotes |
| Treasury curve comparison board helpers | Treasury gallery scripts | `investlabr::factor_yield_curve_*` and `investlabr::viz_*` | Curve date selection, tenor reshaping, and nominal/real curve charting are reusable |
| Strategy explanation views | `strategy-explanation/sim-strategy-explain-*.R` | `strategyr` plus `investlabr::viz_*` | Need reusable plots that explain signals, feature states, position changes, and PnL attribution, not only performance |
| Support/resistance derivation | `market-charts/real-data-yahoo-candles.R` | `strategyr` | Pivot, cycle, Fibonacci, and scoring logic is strategy/technical-analysis logic, not gallery-only plotting |
| Candle chart rendering with S/R overlays | `market-charts/real-data-yahoo-candles.R` | `investlabr::viz_*` | Once S/R points are provided, rendering belongs in investlabr's visualization layer |
| Opportunity-set and frontier summaries | iShares portfolio and opportunity-set scripts | `investlabr::prep_*`, `sim_*`, and `brief_*` | Risk/return grids, efficient-frontier summaries, and embedded tables recur across portfolio research notes |
| Simulation scenario boards | `simulation/sim-*.R` gallery scripts | `investlabr::sim_*` and `investlabr::viz_*` | Scenario data generation and board layout can become reusable research tools |

## Recently Promoted

- Plot-registry entry construction, JSON writing, sidecar loading, and
  validation now live in `R/brief-plot-registry.R`; `scripts/` contains only
  thin executable nodes.
- Forecast series preparation, forward-change extraction, z-scores, bounded
  scores, percentile fans, deterministic scenario paths, and shared forecast
  plots now live in the corresponding `prep_*`, `factor_*`, `sim_*`, `viz_*`,
  and `brief_*` package modules.
- External equity-curve result adaptation and ratio-OHLC construction already
  live in `investlabr::prep_backtest_result_from_equity()` and
  `investlabr::prep_ratio_ohlc()`; `_shared/strategyr-backtest.R` retains only
  local data-access and execution glue.

## Open Design Notes

- Do not move a helper into package code just because it exists once. Wait until it is repeated, hard to test inside a gallery script, or has a stable API.
- Keep local paths, ticker choices, and narrative-specific event labels out of package functions.
- Prefer small composable helpers over one large dashboard function.
- Keep `inst/gallery` scripts executable and readable even after helpers are promoted.
- When code belongs in another package, record that package here rather than forcing it into `investlabr`.

## Strategy Explanation Gap

Current strategy gallery scripts mostly answer: "How did the strategy perform?"

Future strategy-explanation examples should answer:

- What signal condition created each position?
- Which features were active when the strategy entered, exited, or flipped?
- Did returns come from a few trades, persistent exposure, or benchmark beta?
- Where did fees, failed orders, leverage constraints, or tolerance rules matter?
- How did the evaluated strategy differ from buy-and-hold during major drawdowns or rallies?

Likely package boundary:

- `strategyr`: compute signal state, trade-state diagnostics, order/fill diagnostics, and PnL attribution primitives.
- `investlabr`: turn those diagnostics into explanatory charts and briefing-ready summaries.
