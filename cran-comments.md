## Test environments

* local Windows 10 x64 (build 19045), R 4.6.0 ucrt

## R CMD check results

0 errors | 0 warnings | 0 notes

## Resubmission

* corrected the symbolic covariance/correlation method mapping for `method = "B"` and `method = "BG"` and added tests for the published formulas
* added a formal `inst/CITATION` entry and standardized package-side references and dataset provenance in the Rd documentation
* renamed the primary interval time-series interface to `ggInterval_lineplot()` while keeping `ggInterval_tsplot()` as a backward-compatible alias, and replaced the live-download example with a fully reproducible local example
