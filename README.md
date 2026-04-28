# ggInterval

[![CRAN status](https://www.r-pkg.org/badges/version/ggInterval)](https://CRAN.R-project.org/package=ggInterval)
[![GitHub release](https://img.shields.io/github/v/release/hanmingwu1103/ggInterval?include_prereleases)](https://github.com/hanmingwu1103/ggInterval/releases)
[![License: GPL-2+](https://img.shields.io/badge/license-GPL--2%2B-blue.svg)](https://www.gnu.org/licenses/old-licenses/gpl-2.0.en.html)

**ggInterval** extends `ggplot2` for interval-valued data. It provides
plotting functions, data conversion helpers, and examples for symbolic data
analysis workflows where each unit is described by lower and upper bounds
rather than a single point.

The package was previously released as **ggESDA**. The current development line
focuses on interval-valued visualization and companion tools for building
interval data from classical tables.

## Highlights in 0.2.5

- Adds interval-valued correlation heatmaps through `ggInterval_corrplot()`.
- Adds `ggInterval_lineplot()` for interval time-series style displays, with
  `ggInterval_tsplot()` retained as a backward-compatible alias.
- Updates histogram, 2D histogram, scatter, radar, PCA, and index-image
  examples and smoke tests.
- Standardizes package citation metadata and dataset/reference documentation.
- Includes tests for symbolic covariance and correlation method mappings.

## Installation

Install the CRAN release:

```r
install.packages("ggInterval")
```

Install the latest GitHub release:

```r
# install.packages("remotes")
remotes::install_github("hanmingwu1103/ggInterval@v0.2.5")
```

Install the current development version:

```r
# install.packages("remotes")
remotes::install_github("hanmingwu1103/ggInterval")
```

## Quick Start

```r
library(ggInterval)

# Load the built-in interval-valued face measurements
data(facedata)

# Scatter plot
ggInterval_scatterplot(facedata, aes(x = AD, y = BC))

# Histogram
ggInterval_hist(facedata, aes(x = AD), method = "equal-bin", bins = 10)

# 2D histogram with non-equidistant bins
ggInterval_2Dhist(
  facedata,
  aes(x = AD, y = BC),
  method = "unequal-bin"
)$plot

# Correlation heatmap
ggInterval_corrplot(facedata, method = "BG", triangle = "lower")
```

## Main Functions

### Visualization

| Function | Description |
| --- | --- |
| `ggInterval_scatterplot()` | Scatter plot for two interval-valued variables |
| `ggInterval_scatterMatrix()` | Scatter plot matrix for interval-valued variables |
| `ggInterval_3Dscatterplot()` | 3D scatter plot for three interval-valued variables |
| `ggInterval_hist()` | Histogram with equal-bin or unequal-bin construction |
| `ggInterval_2Dhist()` | 2D histogram with equal-bin or unequal-bin construction |
| `ggInterval_2DhistMatrix()` | Matrix of 2D histograms |
| `ggInterval_MMplot()` | Min-max plot |
| `ggInterval_CRplot()` | Center-range plot |
| `ggInterval_indexplot()` | Index plot |
| `ggInterval_indexImage()` | Index image plot |
| `ggInterval_boxplot()` | Boxplot for interval-valued variables |
| `ggInterval_radarplot()` | Radar plot |
| `ggInterval_lineplot()` | Line plot for interval-valued data |
| `ggInterval_PCA()` | PCA display for interval-valued data |
| `ggInterval_corrplot()` | Correlation heatmap for interval-valued variables |

### Data Conversion

| Function | Description |
| --- | --- |
| `classic2sym()` | Convert classical data to interval-valued data |
| `RSDA2sym()` | Convert RSDA symbolic objects to `ggInterval` format |

## Example Graphics

Representative figures generated from `LaTeX/S2_File_ggInterval_examples.R`.

<table>
  <tr>
    <td align="center" width="50%">
      <strong>Interval Scatter Plot</strong><br>
      <sub><code>ggInterval_scatterplot()</code></sub><br>
      <img src="man/figures/README-s2-scatterplot.png" width="100%">
    </td>
    <td align="center" width="50%">
      <strong>Scatter Matrix</strong><br>
      <sub><code>ggInterval_scatterMatrix()</code></sub><br>
      <img src="man/figures/README-s2-scatter-matrix.png" width="100%">
    </td>
  </tr>
  <tr>
    <td align="center" width="50%">
      <strong>2D Histogram Matrix</strong><br>
      <sub><code>ggInterval_2DhistMatrix()</code></sub><br>
      <img src="man/figures/README-s2-2dhist-matrix.png" width="100%">
    </td>
    <td align="center" width="50%">
      <strong>Index Image</strong><br>
      <sub><code>ggInterval_indexImage()</code></sub><br>
      <img src="man/figures/README-s2-index-image.png" width="100%">
    </td>
  </tr>
  <tr>
    <td align="center" width="50%">
      <strong>Customized Index Plot</strong><br>
      <sub><code>ggInterval_indexplot()</code> with k-means groups</sub><br>
      <img src="man/figures/README-s2-custom-index.png" width="100%">
    </td>
    <td align="center" width="50%">
      <strong>Radar Plot</strong><br>
      <sub><code>ggInterval_radarplot()</code></sub><br>
      <img src="man/figures/README-s2-radar.png" width="100%">
    </td>
  </tr>
  <tr>
    <td align="center" width="50%">
      <strong>PCA Display</strong><br>
      <sub><code>ggInterval_PCA()</code> / PCA score intervals</sub><br>
      <img src="man/figures/README-s2-pca.png" width="100%">
    </td>
    <td align="center" width="50%">
      <strong>Correlation Heatmap</strong><br>
      <sub><code>ggInterval_corrplot()</code></sub><br>
      <img src="man/figures/README-s2-corrplot.png" width="100%">
    </td>
  </tr>
  <tr>
    <td align="center" colspan="2">
      <strong>Interval-Valued Line Plot</strong><br>
      <sub><code>ggInterval_lineplot()</code></sub><br>
      <img src="man/figures/README-s2-lineplot.png" width="82%">
    </td>
  </tr>
</table>

## Documentation

- CRAN: <https://CRAN.R-project.org/package=ggInterval>
- GitHub releases: <https://github.com/hanmingwu1103/ggInterval/releases>
- Issues: <https://github.com/hanmingwu1103/ggInterval/issues>

## Citation

Use the installed package citation metadata:

```r
citation("ggInterval")
```

The package citation is shipped with `inst/CITATION` and points to the
canonical CRAN package URL and DOI.

## License

GPL (>= 2)
