# Research Artifact Publishing Contract

`investlabr` owns the production of research plots and consumer-neutral
metadata. It does not own a website content model, page routing, deployment,
or workflow orchestration.

## Produced Artifacts

Local task nodes write generated files under `output/publishing/` and emit a
schema 2.0 registry at `output/publishing/plot-registry.json`. Registry paths
are relative to that output root so local consumers can relocate the complete
tree without rewriting machine-specific paths.

Each registry entry describes the research artifact itself:

- stable plot id and reader-facing text
- research collection, section, asset class, and indicator family
- region, frequency, source, source details, and tags
- latest data date and artifact readiness status
- plot, thumbnail, and optional HTML paths
- related research artifacts and compliance flags

The registry deliberately excludes website slugs, page templates, podcast or
report routing, homepage feature flags, and deployment state.

## Consumer Boundary

A human operator or external workflow orchestrator may run the nodes, read the
registry, transform its entries into a consumer-specific schema, copy selected
artifacts, and invoke downstream publishing commands. That adapter belongs to
the consuming system or private orchestration layer, not to `investlabr`.

The executable node CLI and JSON response form the machine-facing interface.
Gallery scripts referenced by the renderer remain internal research recipes and
may be reorganized without creating a downstream interface change.

No Git-based handoff is required. Consumers may call the nodes against the
local repository and use `--output-root` to select a local exchange directory.

## Compatibility

Writers emit schema 2.0 only. Registry validation accepts schema 1.0 during the
migration period and normalizes these former fields internally:

| Schema 1.0 | Schema 2.0 |
| --- | --- |
| `dashboard` | `collection` |
| `published: true` | `status: ready` |
| `published: false` | `status: draft` |
| `sort_priority` | `curation_priority` |

Consumer-specific schema 1.0 fields that have no research-artifact equivalent
are intentionally not emitted in schema 2.0.
