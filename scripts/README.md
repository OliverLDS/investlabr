# Executable Task Nodes

The scripts in this directory are local, single-purpose task nodes. A human
operator or an external workflow orchestrator may run them directly; workflow
state, scheduling, consumer-specific mapping, and private automation remain
outside `investlabr`.

Executable nodes:

- support `-h` and `--help`
- make inputs and side effects explicit
- perform one conceptual task
- emit stable JSON when producing machine-visible output
- call package functions for reusable behavior
- expose the node CLI, not internal gallery recipe paths, as the stable interface
- avoid personal absolute paths and consumer-specific assumptions

## Nodes

### Render research artifacts

```sh
Rscript scripts/render_plot_assets.R --help
```

Executes explicitly declared gallery scripts as internal rendering recipes and
writes plots and thumbnails under `output/publishing/`. Gallery paths are not
part of the node's public interface. A successful render also writes resolved
runtime freshness metadata under `output/publishing/resolved/`; tracked YAML
sidecars are never rewritten.

### Build the plot registry

```sh
Rscript scripts/build_plot_registry.R --help
```

Joins tracked YAML sidecars from `config/publishing/plots/` with run-local
renderer metadata, validates relative asset paths, and writes
`output/publishing/plot-registry.json`.
The default registry contains only `status: ready` entries; use
`--include-drafts` to include all non-ready entries for local review. Schema
2.0 compatibility output remains the default. Use `--schema-version 3.0` and a
separate `--registry` path for migration testing.

### Validate the plot registry

```sh
Rscript scripts/validate_plot_registry.R --help
```

Validates schemas 1.0, 2.0, or 3.0, controlled values, timestamp ordering,
unique plot ids, and local asset references without modifying files.

`_node-common.R` is a non-executable implementation helper shared by these
nodes; it is not itself a workflow step. See
[`PUBLISHING_CONTRACT.md`](../PUBLISHING_CONTRACT.md) for the downstream
artifact boundary.
