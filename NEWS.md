# ggInterval 0.2.6

## New graphics

* Added four interval diagnostic graphics:
  `ggInterval_ecdf()` displays exact endpoint ECDF envelopes and an optional
  midpoint ECDF; `ggInterval_CRcor()` compares pooled and optional group-demeaned
  Pearson associations among centers and full ranges; `ggInterval_deviation()`
  displays endpoint-deviation contributions; and `ggInterval_overlap()` displays
  geometric overlap between interval boxes.
* Each new function returns an ordinary ggplot object with exact diagnostic
  tables or matrices in its `interval_diagnostics` attribute. They accept
  symbolic tables, ggInterval objects containing `intervalData`, and paired
  lower/upper matrices. They do not change existing plotting interfaces or
  algorithms.
* Added function documentation and tests for interval pairing, empirical CDF
  bounds, zero-width observations, group adjustment, deviation identities and
  overlap geometry. These descriptive graphics do not add inferential tests.

## Data

* Installed the existing aggregation-example CSV under `extdata`, so examples
  can locate it with `system.file()` after package installation. The file is
  unchanged; the documented cleanup removes its empty trailing column.
* Added `stock.data.LH`, a 62-row symbolic table of low-high intervals and
  accompanying date/price columns for the financial line-plot example. It is
  derived from the randomly generated `TTR::ttrc` data (TTR 0.24.4), not an
  actual security. The stored table supports `head()` and the existing
  interval-valued line-plot interface without a run-time TTR dependency.

* Corrected the `INCOMELEVEL` category labels of `Environment` to the order
  of the SODAS source files (25, 75, 50, 90, 100). Earlier versions listed the
  labels as 25, 50, 75, 90, 100 while storing the proportions in the source
  order, so the labels 50 and 75 were interchanged. Compatibility note: only
  these two labels change; all proportions and interval endpoints, and hence
  the radar bar positions and heights, are unchanged.

## Bug fixes

* `ggInterval_PCA(poly = TRUE)` now stops with an informative message when
  duplicated vertices were removed (zero-width intervals or vertices shared
  between observations); it previously failed with `subscript out of bounds`,
  for example for `mushroom`, `blood.i` and `Cardiological`. The polytope
  edge table is now built in one vectorised step: results are identical and
  the `facedata` example runs in a fraction of the previous time.

* Clarified the scatterplot example's transparency settings without changing
  the function's existing mapping precedence or rendered example.
* Clarified that quantile-box widths are heuristic interval-mass display
  weights, and that unseeded K-means examples show one illustrative run.
* Radar layers now map columns from their own layer data. This removes
  discouraged external-data aesthetic mappings and corrects the coordinates
  of two invisible guide paths that previously reused the final observation
  in a loop. The guides remain invisible; the accepted manuscript radar
  displays retain their visible geometry.
* Radar interval-label opacity keys now use an explicit manual scale with
  the existing palette and ordering. The polygon `alpha` argument is
  unchanged; interval labels do not acquire a quantitative interpretation.
* Scatter mappings no longer retain duplicate aesthetics. Existing
  first-mapping precedence is preserved, including the default mapped alpha.
  The three-dimensional scatterplot's fixed coordinate axes no longer
  inherit every observation as data for constant aesthetics.
* The documented RSDA PCA example converts the score-table container to a
  row-name-compatible symbolic data frame before assigning observation
  names. Interval columns, PCA results and plotted labels are retained.
* The ungrouped, single-variable equal-bin histogram now draws its
  precomputed frequencies with `geom_bar(stat = "identity")`. This removes
  warnings about unused `binwidth` and `bins` parameters while retaining
  the existing bin construction, frequency tables and bar geometry.
* Named-column aggregation in `classic2sym()` now passes the grouping
  column through `dplyr::all_of()` to RSDA. This removes the external-vector
  selection deprecation warnings without changing the grouped summaries;
  a missing grouping column still produces an error.
* `ggInterval_hist(method = "equal-bin")`, `ggInterval_2Dhist()` and
  `ggInterval_2DhistMatrix()` now assign a point interval (`min == max`) unit
  mass to exactly one bin. Bins are left-closed and right-open, and the final
  right endpoint belongs to the final bin. Previously a point produced `NaN`
  frequencies in the 1D histogram and was counted in every touching bin in the
  2D histograms (a point on an internal break received mass 4 instead of 1).
  Positive-width intervals keep the existing overlap-length weighting.
* Constant variables (all intervals share one value) no longer fail. In the
  mass-based modes (1D equal-bin and 2D) their support is padded by
  `max(abs(value) * 0.05, 0.5)` on each side (the rule already used by the 2D
  helpers). In the 1D unequal-bin coverage-count display a constant variable,
  including a single observation, is represented by its coverage count at the
  exact value as a zero-width rectangle (vertical spike); no 0.01 grid is built
  for it, so the work does not grow with the magnitude of the value.
* Equal-bin breaks are generated with exactly `bins + 1` values from the pooled
  minimum and maximum, so the last bin can no longer be lost to floating-point
  error.
* `bins`, `xBins` and `yBins` are validated as finite positive integer scalars
  before use in equal-bin mode; the existing bin-count limits are unchanged and
  unequal-bin mode still ignores these arguments.
* The 1D unequal-bin display initialised its first rectangle from the first
  coverage count instead of the first grid index; the first run is now encoded
  correctly when several intervals cover the minimum. Compatibility exception:
  this changes an existing positive-width output. For `facedata` AD the first
  unequal-bin table row changes from `[149.35:150.32] 1` (0.2.5) to
  `[149.34:150.32] 2` (KHA1 and KHA2 both start at 149.34); the other
  `facedata` variables and all equal-bin/2D outputs are unchanged.
* Non-finite interval endpoints are rejected with an explicit error instead of
  propagating `NA`.

## Documentation

* Updated the introductory vignette with direct loading and preview of the
  synthetic `stock.data.LH` table, an optional TTR reconstruction, and
  executable examples of the four diagnostics using the manuscript's
  unchanged simulation mechanism and seed 20260709.
* The vignette now distinguishes histogram mass allocation from coverage
  counts, documents point/constant intervals, explains BG formula provenance
  and normalization, and states the limits of the face and Environment data.
  It also clarifies PCA and radar interpretations and adds layers to the
  returned PCA plot component.

* Histogram help now states the break construction, the overlap weighting,
  the point and endpoint convention, the constant-range padding, and the actual
  returned components; the 1D unequal-bin display is described as a 0.01-grid
  coverage count ("Cumulative Concepts"), which is not a probability.
* `ggInterval_PCA()` documents its actual return value (an augmented
  `princomp` object with `$scores_interval` and `$ggplotPCA`).
* `scale()` documents the class-specific behaviour of the symbolic methods.
* `facedata` documentation describes nine subjects with three image sequences
  per subject (Arce Garro and Rodriguez Rojas, 2019).
* `Environment` and `facedata` documentation describes the verified structure
  and source chain, including the corrected `INCOMELEVEL` label order.

# ggInterval 0.2.5

* CRAN release of 2026-04-28 (see `cran-comments.md` history).
