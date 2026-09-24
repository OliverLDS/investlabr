# CRAN preparation

The package is being prepared for resubmission after feedback on its initial
0.0.6 submission. This document is excluded from the source package. Do not
treat an Actions result as CRAN approval.

CRAN reported five README links to repository-only files absent from the source
tarball and requested a method reference in `DESCRIPTION`. Version 0.0.6.1
replaces those links with repository URLs, cites the mean-variance and GARCH
methods it includes, and corrects the README minimum R version.

## Hosted check result

The [2026-09-23 hosted check run](https://github.com/OliverLDS/investlabr/actions/runs/35895329944)
passed on Linux R release/devel, Windows R release, and macOS R release using
`--as-cran` and a PDF manual build. Windows reported `Status: OK`. The other
three runners reported one NOTE each because HTML Tidy was absent or too old;
there were no package errors or warnings. No CRAN-style check was run on the
maintainer's MacBook.

On 2026-09-23, the official CRAN current-package and archive URLs and the
Bioconductor release-package URL returned 404 for `investlabr`. This is a
point-in-time name check, not a reservation or a guarantee of acceptance.

On 2026-09-24, the maintainer confirmed ownership or redistribution permission
for code adapted from older repositories and curated gallery preview images.
The `URL` and `BugReports` pages in `DESCRIPTION` both returned HTTP 200.

## Implemented

- Public function help includes runnable synthetic examples, shared as source
  under `inst/examples/`. These require neither investdatar nor strategyr.
- Missing imports and data-mask column declarations are addressed.
- The native pipe requires R >= 4.1.0; plotting requires ggplot2 >= 3.5.0.
- SVG regression tests declare svglite in Suggests.
- MIT template metadata is in LICENSE; full terms remain in LICENSE.md.
- The sector gallery no longer saves an image or launches macOS `open` by default.
- Hosted checks cover Linux release/devel and Windows/macOS release, including
  examples, compiled code, tests, and the PDF reference manual.

## Verification policy

Run CRAN-style checks on GitHub Actions, not the maintainer's MacBook. Local
syntax inspection and source-only help regeneration are acceptable:

```r
roxygen2::roxygenise(load_code = roxygen2::load_source)
```

The `R-CMD-check` workflow runs on the preparation branch, on main, and on pull
requests. Check logs are retained as Actions artifacts. Source-only publishing
integration tests can skip in the installed package; the public registry API
tests still run. Real-data gallery recipes remain optional manual workflows.

For the submission candidate, dispatch `CRAN submission candidate` from the
reviewed commit. Its build job uses hosted R release and uploads the source
tarball with a SHA-256 checksum. Its R-devel job verifies that checksum and
checks the same tarball with `R CMD check --as-cran`. Submit only the tarball
from that successful run; do not rebuild it locally.

## Before submission

- Recheck the package name immediately before submission.
- Review `cran-comments.md` against the final source tarball and rerun checks
  if package source changes.
- Consider a short introductory vignette after the required help examples pass.
- Submit only after a final review of the exact source tarball; submission and
  the maintainer's email confirmation remain separate steps.
