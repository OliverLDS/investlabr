# Local Publishing Output

This directory is the local destination for generated research artifacts. It
is repository infrastructure, not installed package content, and generated
files under `output/publishing/` are ignored by Git.

Local consumers run the task nodes in `scripts/` and read the rendered files
directly. No Git-based artifact handoff is required.

## Commands

```sh
Rscript scripts/render_plot_assets.R
Rscript scripts/build_plot_registry.R
Rscript scripts/validate_plot_registry.R
```

## Layout

```text
output/publishing/
├── plot-registry.json
├── plots/
│   ├── macro/
│   ├── markets/
│   ├── crypto/
│   └── strategies/
├── thumbnails/
│   ├── macro/
│   ├── markets/
│   ├── crypto/
│   └── strategies/
└── html/
```

Tracked YAML metadata lives under `config/publishing/plots/`. Asset paths in
those sidecars are relative to `output/publishing/`; absolute paths and parent
directory traversal are rejected.
