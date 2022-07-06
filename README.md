# Open Specy

Analyze, Process, Identify, and Share Raman and (FT)IR Spectra

<!-- badges: start -->
[![CRAN version](https://www.r-pkg.org/badges/version/OpenSpecy)](https://CRAN.R-project.org/package=OpenSpecy) 
[![Project Status](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![R-CMD-check](https://github.com/wincowgerDEV/OpenSpecy-package/workflows/R-CMD-check/badge.svg)](https://github.com/wincowgerDEV/OpenSpecy-package/actions)
[![License: CC BY 4.0](https://img.shields.io/badge/license-CC%20BY%204.0-lightgrey.svg)](https://creativecommons.org/licenses/by/4.0/)
[![DOI](https://img.shields.io/badge/DOI-10.1021/acs.analchem.1c00123-blue.svg)](https://doi.org/10.1021/acs.analchem.1c00123)
[![Website](https://img.shields.io/badge/web-openspecy.org-white)](https://wincowger.shinyapps.io/OpenSpecy/)
[![Twitter Follow](https://img.shields.io/twitter/follow/OpenSpecy)](https://twitter.com/OpenSpecy)
[![Gitter](https://badges.gitter.im/Open-Specy/community.svg)](https://gitter.im/Open-Specy/community?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge)
<!-- badges: end -->

Raman and (FT)IR spectral analysis tool for plastic particles and
other environmental samples (Cowger et al. 2021, doi: 
[10.1021/acs.analchem.1c00123](https://doi.org/10.1021/acs.analchem.1c00123)).
Supported features include reading spectral data files (.asp, .csv, .jdx, .spc,
.spa, .0), Savitzky-Golay smoothing of spectral intensities with
`smooth_intens()`, correcting background noise with `subtr_bg()` in accordance
with Zhao et al. (2007, doi: 
[10.1366/000370207782597003](https://doi.org/10.1366/000370207782597003)), and
identifying spectra using an onboard reference library (Cowger et al. 2020, doi: [10.1177/0003702820929064](https://doi.org/10.1177/0003702820929064)).
Analyzed spectra can be shared with the Open Specy community. A Shiny app is
available via `run_app()` or online at
[https://openanalysis.org/openspecy/](https://openanalysis.org/openspecy/).

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
[package vignette](https://htmlpreview.github.io/?https://github.com/wincowgerDEV/OpenSpecy-package/blob/main/vignettes/sop.html)
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

## Citations

Cowger W, Steinmetz Z, Gray A, Munno K, Lynch J, Hapich H, Primpke S,
De Frond H, Rochman C, Herodotou O (2021). “Microplastic Spectral Classification
Needs an Open Source Community: Open Specy to the Rescue!”
*Analytical Chemistry*, **93**(21), 7543–7548. doi:
[10.1021/acs.analchem.1c00123](https://doi.org/10.1021/acs.analchem.1c00123).

Cowger W, Steinmetz Z (2021). “OpenSpecy: Analyze, Process,
Identify, and Share Raman and (FT)IR Spectra.” *R package*, **0.9.5**.
[https://github.com/wincowgerDEV/OpenSpecy-package](https://github.com/wincowgerDEV/OpenSpecy-package).
