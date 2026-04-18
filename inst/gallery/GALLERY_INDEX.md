# Gallery Index

This index categorizes gallery scripts without moving files. Keep script paths stable for now; use this document as the navigational layer while the gallery grows.

## Category Map

| Category | Scripts | Primary Packages | Promotion Target |
| --- | --- | --- | --- |
| Visualization styles | `viz-style-gallery.R`, `viz-context-gallery.R` | `investlabr` | Keep as gallery showcase |
| Macro data monitor | `real-data-fred-ci-lending-monitor.R`, `real-data-fred-risk-dashboard.R`, `real-data-fred-policy-liquidity-dashboard.R`, `real-data-fred-curve-real-yield-dashboard.R`, `real-data-fred-risk-appetite-dashboard.R`, `real-data-fred-inflation-labor-dashboard.R`, `real-data-fred-liquidity-tightness-dashboard.R`, `real-data-fred-fomc-plumbing-board.R`, `real-data-fred-balance-sheet-mirror-board.R` | `investdatar`, `investlabr` | Reusable `prep_*`, `factor_*`, `brief_*`, and `viz_*` helpers in `investlabr`; source access in `investdatar` |
| Macro data forecast | Implemented: `real-data-fred-rate-shock-persistence-board.R`, `real-data-yahoo-forward-fan-from-recent-regime.R`. Planned/proposed: `real-data-fred-yield-curve-forward-fan.R`, `real-data-fred-policy-rate-path-board.R`, `real-data-fred-real-yield-persistence-board.R`, `real-data-fred-inflation-nowcast-band-board.R`, `real-data-fred-labor-softening-probability-board.R`, `real-data-fred-disinflation-vs-reacceleration-scenarios.R`, `real-data-fred-liquidity-drain-forward-board.R`, `real-data-fred-balance-sheet-runoff-scenarios.R`, `real-data-fred-liquidity-tightness-risk-meter.R`, `real-data-yahoo-macro-regime-conditional-fan-board.R`, `real-data-yahoo-rate-shock-conditional-forward-returns.R`, `real-data-yahoo-dollar-liquidity-spillover-board.R`, `real-data-fred-recession-probability-dashboard.R`, `real-data-fred-soft-landing-vs-hard-landing-board.R`, `real-data-fred-regime-transition-matrix-board.R` | `investdatar`, `investlabr` | Forecast-prep, scenario comparison, fan-chart, probability/regime summary, and briefing helpers in `investlabr`; source access and syncing in `investdatar` |
| Cross-asset and event/regime analysis | `real-data-yahoo-cross-asset-event-board.R`, `real-data-yahoo-us-sector-11-event-board.R`, `real-data-fred-trade-conflict-overlay.R`, `real-data-macro-factor-heatmap.R` | `investdatar`, `investlabr` | `event_*`, `regime_*`, `factor_*`, and `sense_*` helpers in `investlabr` |
| Rates and yield-curve analysis | `real-data-fred-yield-curve.R`, `real-data-treasury-nominal-real-weekly-board.R`, `real-data-treasury-curve-decomposition-board.R` | `investdatar`, `investlabr` | Treasury data access in `investdatar`; curve construction and decomposition helpers in `investlabr` |
| Market chart and technical context | `real-data-yahoo-candles.R` | `investdatar`, `strategyr`, `investlabr` | Support/resistance signal logic in `strategyr`; candle/chart rendering in `investlabr` |
| Portfolio and opportunity-set analysis | `real-data-ishare-portfolio-mix.R`, `real-data-ishare-opportunity-set-board.R` | `investdatar`, `investlabr` | Opportunity-set prep, efficient-frontier summaries, and table-in-plot helpers in `investlabr` |
| Strategy evaluation | `real-data-strategyr-atr-breakout-backtest.R`, `real-data-strategyr-bollinger-backtest.R`, `real-data-strategyr-curve-steepener-backtest.R`, `real-data-strategyr-donchian-backtest.R`, `real-data-strategyr-ema-cross-backtest.R`, `real-data-strategyr-ladder-bounce-backtest.R`, `real-data-strategyr-ladder-breakout-backtest.R`, `real-data-strategyr-macd-backtest.R`, `real-data-strategyr-pair-spread-revert-backtest.R`, `real-data-strategyr-ratio-revert-backtest.R`, `real-data-strategyr-relative-strength-backtest.R`, `real-data-strategyr-rsi-backtest.R`, `real-data-strategyr-rsi-logr-backtest.R`, `real-data-strategyr-trend-pullback-backtest.R`, `real-data-strategyr-vol-target-backtest.R` | `investdatar`, `strategyr`, `investlabr` | Strategy logic and path-dependent diagnostics in `strategyr`; result adapters and explanatory plots in `investlabr` |
| Strategy explanation | `sim-strategy-explain-atr-breakout.R`, `sim-strategy-explain-bollinger-reversion.R`, `sim-strategy-explain-curve-steepener.R`, `sim-strategy-explain-donchian-breakout.R`, `sim-strategy-explain-ema-cross.R`, `sim-strategy-explain-ladder-bounce-breakout.R`, `sim-strategy-explain-macd-cross-contrarian.R`, `sim-strategy-explain-pair-ratio-reversion.R`, `sim-strategy-explain-relative-strength.R`, `sim-strategy-explain-rsi-logr-reversion.R`, `sim-strategy-explain-rsi-reversion.R`, `sim-strategy-explain-trend-pullback.R`, `sim-strategy-explain-vol-target.R` | `strategyr`, `investlabr` | Add more examples that explain signal triggers, feature states, position changes, and PnL attribution |
| Simulation showcase | `sim-digital-option-settlement-mismatch-board.R`, `sim-forward-guidance-vs-warsh-regime.R` | `investlabr` | Reusable `sim_*`, payoff, and scenario-board helpers in `investlabr` |

## Promotion Rule

Promote gallery code when a pattern appears in at least two scripts or when it is clearly a reusable research primitive:

- Data access or source-specific parsing belongs in `investdatar`.
- Strategy signal logic, strategy diagnostics, and path-dependent backtest primitives belong in `strategyr`.
- Research transformations, factor construction, event/regime preparation, scenario summaries, and plot/briefing helpers belong in `investlabr`.
- One-off narrative framing, ticker choices, date windows, and visual examples should stay in `inst/gallery`.

## Script Metadata Convention

Future gallery scripts should make the following easy to identify near the top of the file:

- Purpose: one sentence explaining the research question.
- Data source: local cache provider and required series or symbols.
- Optional sync: commented-out sync lines only.
- Main package functions demonstrated.
- Promotion candidate: whether any helper should later move into `investlabr`, `investdatar`, or `strategyr`.
