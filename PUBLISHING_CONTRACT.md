# Research Artifact Publishing Contract

`investlabr` produces research plots and consumer-neutral metadata. It does not
own website routing, deployment, scheduling, or external workflow state.

## Versioned Registry

Generated assets live under `output/publishing/`. The default registry remains
schema 2.0 at `output/publishing/plot-registry.json` until downstream consumers
complete their schema 3.0 migration. Schema 3.0 can be generated explicitly:

```sh
Rscript scripts/render_plot_assets.R
Rscript scripts/build_plot_registry.R
Rscript scripts/build_plot_registry.R \
  --schema-version 3.0 \
  --registry output/publishing/plot-registry-v3.json
```

Registry paths are relative to the output root. Tracked YAML sidecars under
`config/publishing/plots/` contain human-authored metadata only. Rendering
writes run-local resolved metadata under `output/publishing/resolved/` and does
not rewrite tracked YAML.

## Freshness Fields

Schema 3.0 separates three meanings that schema 2.0 combined:

- `rendered_at` is the UTC `YYYY-MM-DDTHH:MM:SSZ` time when both the concrete
  plot and thumbnail were successfully rendered. Top-level `generated_at` is
  separate and records when the registry itself was generated.
- `data_as_of` is the latest observation actually represented in the artifact.
  It is data-derived and never falls back to the current date, file time,
  render time, or metadata date.
- `metadata_updated_at` is the tracked `YYYY-MM-DD` date when descriptive or
  curation metadata was last materially changed. Rerendering and data sync do
  not change it.

Ready time-indexed artifacts require all three values. A genuinely
non-time-indexed artifact may use `data_as_of: null`. Schema 3.0 forbids
`last_updated`.

Example schema 3.0 entry:

```json
{
  "id": "fred-rate-shock-persistence-board",
  "title": "10-Year Treasury Shock Persistence",
  "subtitle": "Recent nominal-rate shocks versus simulated and realized follow-through",
  "summary": "A rates persistence board.",
  "description_md": "Longer research description.",
  "collection": "macro",
  "section": "rates",
  "asset_class": "Rates",
  "indicator_family": "Rate Shock Persistence",
  "region": "United States",
  "frequency": "Daily",
  "source": "FRED",
  "source_detail": ["DGS10"],
  "tags": ["rates", "treasury"],
  "rendered_at": "2026-08-05T09:15:00Z",
  "data_as_of": "2026-08-04",
  "metadata_updated_at": "2026-08-05",
  "time_indexed": true,
  "status": "ready",
  "curation_priority": 75,
  "plot_image": "plots/macro/fred-rate-shock-persistence-board.svg",
  "thumbnail": "thumbnails/macro/fred-rate-shock-persistence-board.png",
  "plot_html": "",
  "related_ids": [],
  "compliance": {
    "educational_only": true,
    "not_investment_advice": true
  }
}
```

## Conservative Freshness Rules

One scalar date cannot express every source's release lag. Schema 3.0 therefore
uses a conservative deterministic rule: `data_as_of` is the minimum of the
latest usable observation date for every required series or instrument that
contributed to the artifact.

- Mixed-frequency FRED boards use the minimum latest observation across the
  required daily, weekly, and monthly source series before forward filling.
  A release-based weekly or monthly series therefore controls freshness when
  it is the stalest required input.
- Yahoo cross-asset boards use the minimum latest completed bar across every
  required symbol and exchange calendar. A bar dated on the current UTC date is
  excluded unless a recipe can explicitly establish that it is complete.
- Factor heatmaps use the minimum latest complete-case model date across the
  required factor matrix and every instrument included in the reported model.
- Missing values on a latest bar are removed before the per-series maximum is
  calculated. Forward filling can align a plot but cannot advance the source's
  freshness date.

A future schema may add optional per-source freshness details when consumers
need to expose heterogeneous release lags. The scalar schema 3.0 value remains
the conservative artifact-level date.

## Compatibility

Canonical producer metadata uses schema 3.0 semantics. During migration, the
default schema 2.0 writer projects:

| Canonical value | Schema 2.0 output |
| --- | --- |
| `data_as_of` | `last_updated` |
| `rendered_at` | omitted |
| `metadata_updated_at` | omitted |
| `time_indexed` | omitted |

The compatibility writer never infers `rendered_at` or
`metadata_updated_at` from a legacy `last_updated`. Validation continues to
read schemas 1.0 and 2.0. Schema 1.0 field normalization is unchanged.

The exported `brief_plot_registry_entry()` constructor remains available for
legacy schema 2.0 callers and emits a deprecation warning. It cannot be written
as schema 3.0. New canonical entries must be created explicitly with
`brief_plot_registry_entry_v3()` and all required freshness fields.

## Consumer Boundary

An external workflow may run the task nodes, read either registry version,
adapt entries into a consumer-specific schema, copy selected artifacts, and
invoke downstream publishing. Those adapters belong to the consumer or private
orchestration layer, not `investlabr`. No Git-based handoff is required.
