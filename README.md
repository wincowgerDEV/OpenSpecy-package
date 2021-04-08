# Open Specy

Analyze, Process, Identify, and Share, Raman and (FT)IR Spectra

<!-- badges: start -->
[![CRAN version](https://www.r-pkg.org/badges/version/OpenSpecy)](https://CRAN.R-project.org/package=OpenSpecy) 
[![Project Status](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![R-CMD-check](https://github.com/wincowgerDEV/OpenSpecy/workflows/R-CMD-check/badge.svg)](https://github.com/wincowgerDEV/OpenSpecy/actions)
[![License: CC BY 4.0](https://img.shields.io/badge/license-CC%20BY%204.0-lightgrey.svg)](https://creativecommons.org/licenses/by/4.0/)
[![Website](https://img.shields.io/badge/web-openspecy.org-white)](https://wincowger.shinyapps.io/OpenSpecy/)
[![Twitter Follow](https://img.shields.io/twitter/follow/OpenSpecy)](https://twitter.com/OpenSpecy)
<!-- badges: end -->

Raman and (FT)IR spectral analysis tool for plastic particles and
other environmental samples. Supported features include reading spectral
data files (.asp, .csv, .jdx, .spc, .spa, .0), Savitzky-Golay smoothing of
spectral intensities with `smooth_intens()`, correcting background noise with
`subtr_bg()` in accordance with Zhao et al. (2007, doi: 
[dbhfsj](https://doi.org/dbhfsj)),
and identifying spectra using an onboard reference library (Cowger et al. 2020,
doi: [f3sk](https://doi.org/f3sk)). Analyzed spectra can be shared with the Open
Specy community. A Shiny app is available via `run_app()` or online at
[http://www.openspecy.org](https://wincowger.shinyapps.io/OpenSpecy/).

## Installation

**OpenSpecy** is available from CRAN and GitHub.

### Install from CRAN (stable version)

You can install the latest release of **OpenSpecy** from
[CRAN](https://CRAN.R-project.org) with:

```r
install.packages("OpenSpecy")
```

### Install from GitHub (development version)

To install the development version of this package, paste the following code
into your R console (requires **devtools**):

```r
if (!require(devtools)) install.packages("devtools")
devtools::install_github("wincowgerDEV/OpenSpecy")
```

## Getting started
```r
library(OpenSpecy)
run_app()
```

See
[package vignette](https://htmlpreview.github.io/?https://github.com/wincowgerDEV/OpenSpecy/blob/main/vignettes/sop.html)
for a detailed standard operating procedure.

## Workflow

```r
library(dplyr)

# Fetch current spectral library from https://osf.io/x7dpz/
get_lib()

# Load library into global environment
spec_lib <- load_lib()

# Read sample spectrum
raman_hdpe <- read_text(read_extdata("raman_hdpe.csv"))

# Share your spectrum with the Open Spey community
share_spec(raman_hdpe,
           metadata = c(user_name = "Win Cowger",
                        contact_info = "wincowger@gmail.com",
                        spectrum_type = "Raman",
                        spectrum_identity = "HDPE")
           )

# Adjust spectral intensity
raman_adj <- raman_hdpe %>%
  adj_intens()

# Smooth and background-correct spectrum
raman_proc <- raman_adj %>% 
  smooth_intens() %>% 
  subtr_bg()

# Match spectrum with library and retrieve meta data
match_spec(raman_proc, library = spec_lib, which = "raman")

find_spec(sample_name == 5381, library = spec_lib, which = "raman")
```

## Citation

To cite the R package **OpenSpecy** use:

Win Cowger and Zacharias Steinmetz (2021). OpenSpecy: Analyze, Process,
Identify, and Share, Raman and (FT)IR Spectra. R package version 0.8.3.
[https://github.com/wincowgerDEV/OpenSpecy](https://github.com/wincowgerDEV/OpenSpecy)

A BibTeX entry for LaTeX users is

```latex
@Manual{,
  title = {OpenSpecy: Analyze, Process, Identify, and Share, Raman and (FT)IR Spectra},
  author = {Win Cowger and Zacharias Steinmetz},
  year = {2021},
  note = {R package version 0.8.3},
  url = {https://github.com/wincowgerDEV/OpenSpecy},
}
```
