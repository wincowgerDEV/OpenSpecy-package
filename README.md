# Open Specy

Analyze, Process, Identify, and Share, Raman and (FT)IR Spectra

<!-- badges: start -->
[![Project Status](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![R-CMD-check](https://github.com/zsteinmetz/OpenSpecy/workflows/R-CMD-check/badge.svg)](https://github.com/zsteinmetz/OpenSpecy/actions)
[![License: CC BY 4.0](https://img.shields.io/badge/license-CC%20BY%204.0-lightgrey.svg)](https://creativecommons.org/licenses/by/4.0/)
<!-- badges: end -->

## :warning: WARNING :warning:

This R package is currently **not** stable; please use the Shiny app on [openspecy.org](https://wincowger.shinyapps.io/OpenSpecy/) instead.

## Installation

<!-- **OpenSpecy** is available from CRAN and GitHub.

### Install from CRAN (stable version)

You can install the released version of **OpenSpecy** from
[CRAN](https://CRAN.R-project.org) with:

```r
install.packages("envalysis")
```
-->

### Install from GitHub (development version)

To install the development version of this package, paste the following code
into your R console (requires **devtools**):

```r
if (!require(devtools)) install.packages("devtools")
devtools::install_github("zsteinmetz/OpenSpecy")
```

## Getting started

```r
library(OpenSpecy)

# Fetch current spectral library from https://osf.io/x7dpz/
get_lib()

# Load library into global environment
spec_lib <- load_lib()

# Read sample spectrum
data("raman_hdpe")

# Adjust spectral intensity
raman_adj <- adjust_intensity(intensity ~ wavenumber, raman_hdpe)

# Smooth and background-correct spectrum
raman_smoothed <- smooth_intensity(intensity ~ wavenumber, raman_adj)
raman_bgcor <- subtract_background(intensity ~ wavenumber, raman_smoothed)

# Match spectrum with library
match_spectrum(raman_bgcor, spec_lib, "raman", "full")
```

