# Gallery Index

Gallery examples are physically organized by research purpose. Paths below are
relative to `inst/gallery/` and remain user-facing installed examples.

## Category Map

| Category | Scripts | Primary Packages | Promotion Target |
| --- | --- | --- | --- |
| Visualization styles | `visualization/viz-style-gallery.R`, `visualization/viz-context-gallery.R` | `investlabr` | Keep as gallery showcase |
| Macro data monitor | `macro-monitor/real-data-fred-ci-lending-monitor.R`, `macro-monitor/real-data-fred-risk-dashboard.R`, `macro-monitor/real-data-fred-policy-liquidity-dashboard.R`, `rates/real-data-fred-curve-real-yield-dashboard.R`, `macro-monitor/real-data-fred-risk-appetite-dashboard.R`, `macro-monitor/real-data-fred-inflation-labor-dashboard.R`, `macro-monitor/real-data-fred-liquidity-tightness-dashboard.R`, `macro-monitor/real-data-fred-fomc-plumbing-board.R`, `macro-monitor/real-data-fred-balance-sheet-mirror-board.R` | `investdatar`, `investlabr` | Reusable `prep_*`, `factor_*`, `brief_*`, and `viz_*` helpers in `investlabr`; source access in `investdatar` |
| Macro data forecast | `macro-forecast/real-data-fred-rate-shock-persistence-board.R`, `macro-forecast/real-data-yahoo-forward-fan-from-recent-regime.R`, `macro-forecast/real-data-fred-yield-curve-forward-fan.R`, `macro-forecast/real-data-fred-policy-rate-path-board.R`, `macro-forecast/real-data-fred-real-yield-persistence-board.R`, `macro-forecast/real-data-fred-inflation-nowcast-band-board.R`, `macro-forecast/real-data-fred-labor-softening-probability-board.R`, `macro-forecast/real-data-fred-disinflation-vs-reacceleration-scenarios.R`, `macro-forecast/real-data-fred-liquidity-drain-forward-board.R`, `macro-forecast/real-data-fred-balance-sheet-runoff-scenarios.R`, `macro-forecast/real-data-fred-liquidity-tightness-risk-meter.R`, `macro-forecast/real-data-yahoo-macro-regime-conditional-fan-board.R`, `macro-forecast/real-data-yahoo-rate-shock-conditional-forward-returns.R`, `macro-forecast/real-data-yahoo-dollar-liquidity-spillover-board.R`, `macro-forecast/real-data-fred-recession-probability-dashboard.R`, `macro-forecast/real-data-fred-soft-landing-vs-hard-landing-board.R`, `macro-forecast/real-data-fred-regime-transition-matrix-board.R` | `investdatar`, `investlabr` | Forecast-prep, scenario comparison, fan-chart, probability/regime summary, and briefing helpers in `investlabr`; source access and syncing in `investdatar` |
| Cross-asset and event/regime analysis | `cross-asset/real-data-yahoo-cross-asset-event-board.R`, `cross-asset/real-data-yahoo-us-sector-11-event-board.R`, `cross-asset/real-data-fred-trade-conflict-overlay.R`, `cross-asset/real-data-macro-factor-heatmap.R` | `investdatar`, `investlabr` | `event_*`, `regime_*`, `factor_*`, and `sense_*` helpers in `investlabr` |
| Rates and yield-curve analysis | `rates/real-data-fred-yield-curve.R`, `rates/real-data-fred-curve-real-yield-dashboard.R`, `rates/real-data-treasury-nominal-real-weekly-board.R`, `rates/real-data-treasury-curve-decomposition-board.R` | `investdatar`, `investlabr` | Treasury data access in `investdatar`; curve construction and decomposition helpers in `investlabr` |
| Market chart and technical context | `market-charts/real-data-yahoo-candles.R`, `market-charts/real-data-yahoo-volatility-clustering-board.R` | `investdatar`, `strategyr`, `investlabr` | Support/resistance signal logic in `strategyr`; chart rendering and market diagnostics in `investlabr` |
| Portfolio and opportunity-set analysis | `portfolio-research/real-data-ishare-portfolio-mix.R`, `portfolio-research/real-data-ishare-opportunity-set-board.R` | `investdatar`, `investlabr` | Opportunity-set prep, efficient-frontier summaries, and table-in-plot helpers in `investlabr` |
| Strategy evaluation | `strategy-evaluation/real-data-strategyr-atr-breakout-backtest.R`, `strategy-evaluation/real-data-strategyr-bollinger-backtest.R`, `strategy-evaluation/real-data-strategyr-curve-steepener-backtest.R`, `strategy-evaluation/real-data-strategyr-donchian-backtest.R`, `strategy-evaluation/real-data-strategyr-ema-cross-backtest.R`, `strategy-evaluation/real-data-strategyr-ladder-bounce-backtest.R`, `strategy-evaluation/real-data-strategyr-ladder-breakout-backtest.R`, `strategy-evaluation/real-data-strategyr-macd-backtest.R`, `strategy-evaluation/real-data-strategyr-pair-spread-revert-backtest.R`, `strategy-evaluation/real-data-strategyr-ratio-revert-backtest.R`, `strategy-evaluation/real-data-strategyr-relative-strength-backtest.R`, `strategy-evaluation/real-data-strategyr-rsi-backtest.R`, `strategy-evaluation/real-data-strategyr-rsi-logr-backtest.R`, `strategy-evaluation/real-data-strategyr-trend-pullback-backtest.R`, `strategy-evaluation/real-data-strategyr-vol-target-backtest.R` | `investdatar`, `strategyr`, `investlabr` | Strategy logic and path-dependent diagnostics in `strategyr`; result adapters and explanatory plots in `investlabr` |
| Strategy explanation | `strategy-explanation/sim-strategy-explain-atr-breakout.R`, `strategy-explanation/sim-strategy-explain-bollinger-reversion.R`, `strategy-explanation/sim-strategy-explain-curve-steepener.R`, `strategy-explanation/sim-strategy-explain-donchian-breakout.R`, `strategy-explanation/sim-strategy-explain-ema-cross.R`, `strategy-explanation/sim-strategy-explain-ladder-bounce-breakout.R`, `strategy-explanation/sim-strategy-explain-macd-cross-contrarian.R`, `strategy-explanation/sim-strategy-explain-pair-ratio-reversion.R`, `strategy-explanation/sim-strategy-explain-relative-strength.R`, `strategy-explanation/sim-strategy-explain-rsi-logr-reversion.R`, `strategy-explanation/sim-strategy-explain-rsi-reversion.R`, `strategy-explanation/sim-strategy-explain-trend-pullback.R`, `strategy-explanation/sim-strategy-explain-vol-target.R` | `strategyr`, `investlabr` | Add more examples that explain signal triggers, feature states, position changes, and PnL attribution |
| Simulation showcase | `simulation/sim-digital-option-settlement-mismatch-board.R`, `simulation/sim-forward-guidance-vs-warsh-regime.R` | `investlabr` | Reusable `sim_*`, payoff, and scenario-board helpers in `investlabr` |

## Adjacent Repository Surfaces

- `scripts/` contains local executable task nodes for rendering, registry
  generation, and validation. These are not gallery examples and are excluded
  from the installed package.
- `config/publishing/plots/` contains tracked, consumer-neutral plot metadata.
- `output/publishing/` contains generated local publishing artifacts and is
  ignored by Git.
- `inst/gallery/assets/` contains only curated previews that should ship with
  the package.
- `_shared/` contains lightweight example glue. Reusable research logic belongs
  in the package module families under `R/`.

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
