# investlabr Package Scope

## Purpose

`investlabr` is an R package for **macro-financial and cross-asset research workflows**.

It sits **on top of data packages** such as `investdatar` and provides reusable tools for:

- preparing research-ready datasets
- constructing macro and market factors
- analyzing events and regimes
- estimating sensitivities and exposures
- running scenario and structural-break simulations
- producing research-grade charts, dashboards, and briefing outputs

In simple terms:

- `investdatar` gets and stores the data
- `investlabr` thinks with the data

## Core Positioning

`investlabr` is **not** a raw data collection package and **not** a trading execution package.

Its role is to standardize the layer between:

1. data availability
2. research interpretation
3. presentation-ready output

The package should be optimized for workflows such as:

- macro dashboard building
- cross-asset comparison
- event-window and regime analysis
- factor sensitivity analysis
- structural scenario comparison
- meeting brief / market note preparation

## In-Scope Areas

### 1. Research Data Preparation

Functions that turn already-fetched market and macro series into analysis-ready datasets.

Typical responsibilities:

- align series to a common calendar
- merge data from different frequencies
- reshape between long and wide formats
- compute indexed series
- compute YoY, MoM, rolling changes, spreads, and derived indicators
- prepare panel-ready tables for plotting and reporting

Examples of likely function families:

- `prep_align_calendar_*()`
- `prep_long_wide_*()`
- `prep_index100_*()`
- `prep_growth_*()`
- `prep_spread_*()`
- `prep_panel_*()`

### 2. Macro and Cross-Asset Diagnostics

Functions that help study the joint behavior of rates, yield curves, inflation compensation, dollar and FX, commodities, equities, volatility, liquidity and reserves, and labor and macro conditions.

Typical responsibilities:

- build cross-asset comparison tables
- derive curve shape measures
- compare macro indicators against market prices
- create standardized diagnostics for policy, liquidity, inflation, and risk sentiment

Examples of likely function families:

- `factor_yield_curve_*()`
- `factor_liquidity_*()`
- `factor_inflation_*()`
- `factor_cross_asset_*()`
- `diag_macro_*()`
- `diag_market_*()`

### 3. Event and Regime Analysis

Functions for adding time-based interpretation layers to market and macro data.

Typical responsibilities:

- define event windows
- define recurring policy dates
- define regime periods
- expand event tables across facets or assets
- compare before/after behavior
- overlay shaded periods and event markers on charts
- support event-study style summaries

Examples of likely function families:

- `event_define_*()`
- `event_expand_*()`
- `event_window_*()`
- `regime_define_*()`
- `regime_compare_*()`
- `viz_event_overlay_*()`

### 4. Sensitivity and Exposure Analysis

Functions that estimate how assets or sectors respond to macro factors.

Typical responsibilities:

- compute full-sample factor sensitivities
- compute rolling betas / time-varying exposures
- compare exposure profiles across assets
- summarize which macro factors dominate a stock, sector, or theme
- standardize exposure outputs into tidy tables and charts

Examples of likely function families:

- `sense_beta_*()`
- `sense_rolling_beta_*()`
- `sense_factor_exposure_*()`
- `sense_compare_assets_*()`
- `sense_summary_*()`

### 5. Scenario and Structural Simulation

Functions for conceptual or applied scenario analysis.

Typical responsibilities:

- simulate structural breaks
- compare alternative policy regimes
- map payoff profiles under alternative outcomes
- test how assumptions change results
- create scenario-ready outputs for research notes

Examples of likely function families:

- `sim_structural_break_*()`
- `sim_policy_regime_*()`
- `sim_curve_path_*()`
- `sim_payoff_*()`
- `sim_scenario_grid_*()`

### 6. Visualization and Briefing Output

Functions that convert research objects into publication-ready output.

Typical responsibilities:

- build consistent ggplot-based charts
- assemble multi-panel dashboards
- apply standard labels, themes, and annotations
- generate briefing figures for macro and market reviews
- create compact research summary tables
- create text-ready or prompt-ready summaries for later narrative generation

Examples of likely function families:

- `viz_line_*()`
- `viz_panel_*()`
- `viz_dashboard_*()`
- `viz_regime_*()`
- `brief_table_*()`
- `brief_summary_*()`

## Explicitly Out of Scope

### 1. Raw Data Collection and Syncing

`investlabr` should not own data downloading, source registry management, or local storage synchronization.

These belong in packages such as:

- `investdatar`

### 2. Agent Memory, LLM Orchestration, or Workflow Automation

If a script uses an agent or LLM, `investlabr` should only own the research transformation layer, not the agent system itself.

Out of scope:

- persistent agent memory
- autonomous workflow orchestration
- model routing
- prompt execution engines

### 3. Trade Execution, Broker Integration, and Stateful Backtesting

`investlabr` is not a live trading engine.

Out of scope:

- order placement
- broker API execution
- portfolio order management
- stateful trade simulation
- production trading automation

### 4. Portfolio Accounting / Holdings Administration

Out of scope:

- transaction ledger management
- NAV accounting
- tax lot tracking
- reconciliation
- custodial workflows

## Package Boundary Relative to Other Packages

### `investdatar`

Owns:

- downloading
- syncing
- source metadata
- local storage
- source-specific parsing

### `investlabr`

Owns:

- preparation of analysis-ready research tables
- factor and indicator construction
- event/regime overlays
- sensitivity estimation
- simulation and scenario comparison
- charting and briefing outputs

### LLM-related or orchestration tools

Own:

- orchestration
- prompts
- summarization
- narrative generation
- memory and autonomous workflow logic

### Trading packages

Own:

- signal execution
- orders
- paper trading
- backtesting
- broker integration

## Suggested Internal Module Families

- `prep-*.R`
- `factor-*.R`
- `event-*.R`
- `regime-*.R`
- `sense-*.R`
- `sim-*.R`
- `viz-*.R`
- `brief-*.R`

Possible examples:

- `prep-calendar.R`
- `prep-transforms.R`
- `factor-yieldcurve.R`
- `factor-liquidity.R`
- `event-overlay.R`
- `regime-compare.R`
- `sense-betas.R`
- `sim-structural.R`
- `viz-dashboards.R`
- `brief-summary.R`

## Design Principles for Codex

1. Keep the package focused on research workflows, not data acquisition.
2. Prefer tidy, reusable transformation functions over one-off scripts.
3. Return objects that are easy to reuse:
   - `data.table`
   - tidy tables
   - `ggplot` objects
   - small named lists of outputs
4. Separate data preparation, analytic logic, and visualization.
5. Avoid hard-coding file paths, agent objects, or personal local directories.
6. Avoid mixing package code with narrative-generation code.
7. Make functions composable for dashboards and research notes.
8. Prefer names that reflect workflow purpose clearly:
   - `prep_*`
   - `factor_*`
   - `event_*`
   - `sense_*`
   - `sim_*`
   - `viz_*`
   - `brief_*`

## Practical Test for Scope

A new function likely belongs in `investlabr` if the answer to this question is yes:

> Does this function help turn existing macro/market data into a reusable research insight, comparison, scenario, or visual output?

If yes, it is probably in scope.

If it mainly downloads data, routes LLM prompts, sends orders, or manages portfolios, it is probably out of scope.
