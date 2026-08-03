# Plot Registry Metadata

`plots/` contains one tracked YAML sidecar per research-artifact candidate.
These files describe the chart and its source data; they do not contain
website routes, page slugs, editorial destinations, or deployment state.

The local registry node reads this directory and writes schema 2.0 output:

```sh
Rscript scripts/build_plot_registry.R
```

Each sidecar uses relative `plot_image`, `thumbnail`, and optional `plot_html`
paths rooted at `output/publishing/`. Use `status: draft`, `status: ready`, or
`status: archived` to describe artifact readiness.
