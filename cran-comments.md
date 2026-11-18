## Submission

Update of ggInterval from 0.2.5 (on CRAN) to 0.2.6.

## Changes

See NEWS.md. In brief:

* four descriptive diagnostic graphics: `ggInterval_ecdf()`,
  `ggInterval_CRcor()`, `ggInterval_deviation()` and `ggInterval_overlap()`;
* a small synthetic example dataset, `stock.data.LH`;
* corrected `INCOMELEVEL` category labels in the `Environment` dataset
  (labels only; every stored proportion and interval endpoint is unchanged);
* point-interval and constant-variable fixes in the histogram functions;
* `ggInterval_PCA(poly = TRUE)`: an informative error for incomplete vertex
  sets and a vectorised edge table (identical results; the example now runs
  well below the 5 s example limit that an earlier build reached once);
* documentation and vignette updates.

## Test environments

* Ubuntu 24.04.5 LTS, x86_64, R 4.5.3 (conda-forge toolchain):
  `R CMD check --as-cran --run-donttest --no-multiarch`.
* Windows 11 x64 (build 26300), R version 4.6.2 (2026-10-29 ucrt):
  the same command, on the submitted archive.
* Windows 11 x64 (build 26300), R Under development (unstable)
  (2026-11-14 r90890 ucrt): the same command, on the submitted archive.

## R CMD check results

Windows 11 x64, R 4.6.2: 0 errors | 0 warnings | 0 notes.
Tests: 1218 passed, 0 failed, 0 skipped, 0 warnings.
Slowest example 1.0 s (`ggInterval_radarplot`); `ggInterval_PCA` 0.4 s.

Windows 11 x64, R-devel (r90890): 0 errors | 0 warnings | 0 notes.
Tests: 1218 passed, 0 failed, 0 skipped, 0 warnings.
Slowest example 0.9 s (`ggInterval_radarplot`); `ggInterval_PCA` 0.4 s.

Both Windows checks were run on the submitted archive (built with R 4.6.2)
with the remote incoming checks enabled. The example output contains no
warnings.

Ubuntu 24.04.5, R 4.5.3: 0 errors | 0 warnings | 0 notes.
Tests: 1218 passed, 0 failed, 0 skipped. Slowest example 2.0 s.

Notes on that offline machine: the archive was built there from the same
sources, the network parts of the incoming checks were switched off
(`_R_CHECK_CRAN_INCOMING_REMOTE_=false`, `_R_CHECK_SYSTEM_CLOCK_=0`), and
the PDF manual was built with `R_RD4PDF="times,hyper"` because the
inconsolata font is not installed there. One test prints the platform
message "no DISPLAY variable so Tk is not available" on that headless
machine only.

## Reverse dependencies

dataSDA 0.2.7.4 (Suggests: ggInterval) is the only reverse dependency on
CRAN. Its released archive was checked under Windows R 4.6.2 with
`R CMD check --as-cran --run-donttest --no-multiarch`, once with ggInterval
0.2.5 and once with the submitted 0.2.6 archive installed. The two runs give
the same result: 0 errors, 1 warning, 0 notes; tests 1065 passed, 0 failed,
14 skipped ("On CRAN"). The warning is the incoming-check message
"Insufficient package version (submitted: 0.2.7.4, existing: 0.2.7.4)", which
appears only because the already published dataSDA archive was checked again;
it does not depend on ggInterval. The example and test logs of the two runs
differ only in their timing lines.

dataSDA skips its optional ggInterval vignette chunks on CRAN, so the four
chunks that call ggInterval (aggregation, index image, PCA, radar plot) were
also run directly with both versions. All four ran without error under both;
the interval and PCA results are identical and the three plots are identical
when rendered at 120 dpi. The radar chunk gave 30 warnings with 0.2.5 and
none with 0.2.6.

dataSDA carries its own copy of the `Environment` data (`environment.mix`,
still with the earlier `INCOMELEVEL` labels) and does not compare it with
`ggInterval::Environment`, so the label correction causes no new issue.

## Vignette

The archive is built with `--no-build-vignettes` so that the prebuilt
vignette, whose clustering examples are unseeded, keeps its images and printed
results. The printed `Environment` outputs in the prebuilt HTML were updated
to the corrected labels and agree with the output of the current code.
`R CMD check` re-runs the vignette code.
