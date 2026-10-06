## Resubmission

This resubmission addresses the feedback on version 0.0.6.1:

* Removed default relative output paths from both exported plot-registry
  writers in `R/brief-plot-registry.R`; callers must now supply `path`.
* Confirmed that their examples and tests write only to temporary directories.
* Added regression tests for the missing-path behavior and increased the
  package version to 0.0.6.2.

## Test environments

* GitHub Actions, Ubuntu latest, R release and R-devel
* GitHub Actions, Windows latest, R release
* GitHub Actions, macOS latest, R release

The exact source tarball is built on hosted R release and checked on hosted
R-devel with `R CMD check --as-cran`. The run URL is included in the CRAN
submission form comment.

## R CMD check results

The candidate workflow requires no errors, no warnings, and no notes other
than the expected CRAN incoming-feasibility "New submission" note.

## Downstream dependencies

There are no known reverse dependencies; this is an initial CRAN release.
