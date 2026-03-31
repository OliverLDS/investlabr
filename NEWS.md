# investlabr news

## 0.0.1

- Repositioned `investlabr` as a macro-financial and cross-asset research workflow package.
- Clarified package scope, boundaries, and design principles relative to `investdatar`.
- Reorganized the R source tree into `prep-*`, `factor-*`, `event-*`, `regime-*`, `sense-*`, `sim-*`, `viz-*`, and `brief-*` module families.
- Preserved legacy exported functions during the structural migration so downstream code can continue to work while the API evolves.
- Added package-level documentation for scope, architecture, and testing.
- Added a basic `testthat` harness with smoke tests for key package workflows.
