# Plot Registry Metadata

`plots/` contains one tracked YAML sidecar per research-artifact candidate.
These files describe the chart and its source data; they do not contain
website routes, page slugs, editorial destinations, or deployment state.

The local registry node joins this directory with run-local resolved metadata
written by the renderer. It writes schema 2.0 compatibility output by default:

```sh
Rscript scripts/build_plot_registry.R
```

Each sidecar uses relative `plot_image`, `thumbnail`, and optional `plot_html`
paths rooted at `output/publishing/`. Use `status: draft`, `status: ready`, or
`status: archived` to describe artifact readiness. `metadata_updated_at` is the
date of the latest material human-authored metadata change, `time_indexed`
declares whether a ready artifact requires a data-derived freshness date, and
`expected_cadence` declares the producer-owned update cadence of that date.
Use `daily`, `weekly`, `monthly`, `event_driven`, or `not_time_indexed`.
Cadence follows the slowest required input for the artifact unless an explicit
event-driven rule is documented.

Do not place `rendered_at`, `data_as_of`, or legacy `last_updated` in tracked
sidecars. The renderer writes the first two under
`output/publishing/resolved/` without modifying this directory.
