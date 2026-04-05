# investlabr Architecture

## Purpose

`investlabr` is the research-transformation layer that sits above data-access packages such as `investdatar`.

The package should take already-available market or macro data and turn it into:

- analysis-ready tables
- reusable research signals or factors
- event and regime comparisons
- scenario and sensitivity outputs
- publication-ready charts and briefing objects

Relative to neighboring packages:

- `investdatar` is the data-access and local-sync layer
- `strategyr` is the execution-oriented strategy and dynamic backtesting layer
- `investlabr` is the exploratory research, visualization, and communication layer

## Design Boundary

`investlabr` should own:

- preparation of research-ready datasets
- factor construction and derived indicators
- event-study and regime overlays
- sensitivity and exposure estimation
- scenario comparison and structural simulation
- visualization and briefing outputs

`investlabr` should not own:

- data downloading or sync infrastructure
- provider-specific parsing
- agent memory or orchestration engines
- broker execution or portfolio accounting

`investlabr` may keep lightweight backtesting helpers where they help explain a market pattern, compare a simple hypothesis, or build a visual briefing object. When the problem becomes "what should the strategy do right now?" with realistic, path-dependent, execution-grade logic, that belongs in `strategyr`.

## Module Families

### `prep-*`

Research data preparation. Typical responsibilities:

- align calendars
- reshape long and wide tables
- compute indexed and growth-transformed series
- produce panel-ready tables

### `factor-*`

Macro and cross-asset factor construction. Typical responsibilities:

- yield curve measures
- liquidity indicators
- inflation or cross-asset comparison tables

### `event-*`

Event-study style analytics. Typical responsibilities:

- event-window return calculations
- event expansion and before/after comparison tables

### `regime-*`

Regime definitions and comparisons. Typical responsibilities:

- period labeling
- regime overlays
- regime-by-regime comparisons

### `sense-*`

Sensitivity and exposure analysis. Typical responsibilities:

- full-sample betas
- rolling exposures
- factor-exposure summaries

### `sim-*`

Scenario and structural simulation. Current status:

- includes legacy backtesting and portfolio-simulation helpers from the older package scope
- should gradually broaden toward scenario and structural comparison tools
- should keep any retained backtesting helpers clearly framed as exploratory rather than execution-grade

### `viz-*`

Reusable plotting and dashboard helpers. Typical responsibilities:

- market plots
- event plots
- panel layouts
- dashboard composition

### `brief-*`

Briefing-oriented outputs. Typical responsibilities:

- summary plots
- compact tables
- note-ready research objects

## Current State

The repository is in a transition state.

- The directory structure already follows the target module families.
- Some exported functions still use older names such as `eval_*` and `gen_*`.
- The current `sim-*` files still contain legacy strategy-evaluation helpers. They remain temporarily for compatibility and should be refactored or wrapped into broader research workflows over time.
- A few gallery examples still read niche upstream sources directly when `investdatar` does not yet expose them as first-class local source adapters. Those cases should be treated as temporary. Treasury XML yield-curve data is one concrete example that should eventually move into `investdatar`, with `investlabr` consuming the synchronized local output rather than parsing the source directly.

## Implementation Principles

- Prefer pure or near-pure functions with explicit inputs and outputs.
- Prefer `data.table`, tidy tables, `ggplot` objects, or small named lists as return values.
- Keep data preparation, analytical logic, and visualization separate.
- Avoid hard-coded local paths, agent objects, or project-specific runtime state.
- Preserve backward compatibility where practical during migration.

## Testing Strategy

Package-level tests should focus on:

- shape and type guarantees for core data transformations
- smoke coverage for exported plotting functions
- compatibility coverage for legacy public functions retained during migration
- low-dependency fixtures that do not rely on live data downloads
