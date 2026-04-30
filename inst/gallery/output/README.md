# Gallery Output

This folder holds rendered gallery artifacts and publishing-side metadata.

## AlphaSync Registry

`investlabr` can emit an AlphaSync-compatible manifest at:

- `inst/gallery/output/alphasync-plot-registry.json`

The recommended command from the package root is:

```r
source("inst/gallery/build-alphasync-real-assets.R")
source("inst/gallery/build-alphasync-plot-registry.R")
```

Use `build-alphasync-real-assets.R` when you want `investlabr` to render the
selected gallery boards into `plots/` and `thumbnails/` before building the
registry. Use `build-alphasync-plot-registry.R` when the assets and YAML
sidecars already exist and only the manifest needs to be refreshed.

## Expected Layout

The intended structure is:

```text
inst/gallery/output/
├── alphasync-plot-registry.json
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
├── html/
│   ├── macro/
│   ├── markets/
│   ├── crypto/
│   └── strategies/
└── meta/
    └── *.yaml
```

`plots/`, `thumbnails/`, and `html/` are created on demand by workflow
scripts. The `meta/` directory is expected to contain one YAML file per
published chart.

## Metadata Sidecar Convention

Each YAML file in `inst/gallery/output/meta/` should describe one plot using
the AlphaSync registry schema. Required fields are:

- `id`
- `title`
- `dashboard`
- `asset_class`
- `indicator_family`
- `region`
- `frequency`
- `source`
- `last_updated`
- `published`
- `plot_image`
- `thumbnail`
- `tags`

Recommended optional fields are:

- `subtitle`
- `summary`
- `description_md`
- `section`
- `source_detail`
- `plot_html`
- `canonical_slug`
- `related_ids`
- `podcast_topics`
- `report_topics`
- `featured`
- `sort_priority`
- `compliance`

## Controlled Labels

The current validator enforces the following controlled values:

- `dashboard`: `macro`, `markets`, `crypto`, `strategies`
- `frequency`: `Daily`, `Weekly`, `Monthly`, `Quarterly`, `Event`
- `asset_class`: `Rates`, `Liquidity`, `Inflation`, `Equities`, `Bonds`,
  `FX`, `Commodities`, `Crypto`, `Strategy Signals`
- `region`: `United States`, `Europe`, `Japan`, `China`, `Global`,
  `Emerging Markets`

Paths in sidecars must always be relative to `inst/gallery/output/`, never
absolute local machine paths.
