# Gallery Promotion Backlog

Use this file as a lightweight backlog for patterns that emerge from daily-analysis gallery work. The goal is to keep scripts useful while gradually moving reusable logic into the right package.

## Near-Term Candidates

| Candidate | Current Location | Suggested Target | Reason |
| --- | --- | --- | --- |
| Local FRED loading helpers | Several `real-data-fred-*.R` scripts | `investlabr::prep_*` or `investdatar` | Repeated `get_local_FRED_data()` normalization to `date`, numeric `value`, and labeled long tables |
| Macro monitor panel builders | `real-data-fred-ci-lending-monitor.R`, policy/liquidity dashboards | `investlabr::brief_*` or `investlabr::viz_*` | Repeated multi-panel monitor pattern with local FRED cache, themed plots, and reader-facing footnotes |
| Macro forecast helpers | `macro-forecast-gallery-utils.R` and `real-data-fred/yahoo-*forecast/scenario/probability/regime*` scripts | `investlabr::prep_*`, `factor_*`, `sim_*`, `regime_*`, `brief_*`, and `viz_*` | Forward-looking macro research now repeats local series normalization, forward fans, deterministic scenario paths, bounded risk meters, and regime-transition summaries while data access stays in `investdatar` |
| Treasury curve comparison board helpers | Treasury gallery scripts | `investlabr::factor_yield_curve_*` and `investlabr::viz_*` | Curve date selection, tenor reshaping, and nominal/real curve charting are reusable |
| Strategyr result adapters | `strategyr-backtest-gallery-utils.R` and strategy gallery scripts | `investlabr::prep_*` and `investlabr::eval_*` | External backtest outputs need stable conversion into research-grade equity plots |
| Strategy explanation views | `sim-strategy-explain-*.R` | `strategyr` plus `investlabr::viz_*` | Need reusable plots that explain signals, feature states, position changes, and PnL attribution, not only performance |
| Support/resistance derivation | `real-data-yahoo-candles.R` | `strategyr` | Pivot, cycle, Fibonacci, and scoring logic is strategy/technical-analysis logic, not gallery-only plotting |
| Candle chart rendering with S/R overlays | `real-data-yahoo-candles.R` | `investlabr::viz_*` | Once S/R points are provided, rendering belongs in investlabr's visualization layer |
| Opportunity-set and frontier summaries | iShares portfolio and opportunity-set scripts | `investlabr::prep_*`, `sim_*`, and `brief_*` | Risk/return grids, efficient-frontier summaries, and embedded tables recur across portfolio research notes |
| Simulation scenario boards | `sim-*.R` gallery scripts | `investlabr::sim_*` and `investlabr::viz_*` | Scenario data generation and board layout can become reusable research tools |

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
