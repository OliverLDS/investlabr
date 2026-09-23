## Test environments

* GitHub Actions, Ubuntu latest, R release and R-devel
* GitHub Actions, Windows latest, R release
* GitHub Actions, macOS latest, R release

The checks used `R CMD check --as-cran`, built the PDF reference manual, and
ran examples and the complete test suite. Results are recorded in
https://github.com/OliverLDS/investlabr/actions/runs/35895329944 .

## R CMD check results

* Windows release: 0 errors, 0 warnings, 0 notes.
* Linux release/devel and macOS release: 0 errors, 0 warnings, 1 note each.

The only note on these runners was that HTML validation was skipped because
HTML Tidy was unavailable or too old in the runner environment. The HTML
manual and PDF manual otherwise built successfully.

## Downstream dependencies

There are no known reverse dependencies; this is an initial CRAN submission.
