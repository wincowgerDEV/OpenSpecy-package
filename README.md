# Open Specy 1.0

Analyze, Process, Identify, and Share Raman and (FT)IR Spectra

<!-- badges: start -->
[![CRAN version](https://www.r-pkg.org/badges/version/OpenSpecy)](https://CRAN.R-project.org/package=OpenSpecy) 
[![Project Status](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![R-CMD-check](https://github.com/wincowgerDEV/OpenSpecy-package/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/wincowgerDEV/OpenSpecy-package/actions/workflows/R-CMD-check.yaml)
[![Codecov test coverage](https://codecov.io/gh/wincowgerDEV/OpenSpecy-package/branch/main/graph/badge.svg)](https://app.codecov.io/gh/wincowgerDEV/OpenSpecy-package?branch=main)
[![License: CC BY 4.0](https://img.shields.io/badge/license-CC%20BY%204.0-lightgrey.svg)](https://creativecommons.org/licenses/by/4.0/)
[![DOI](https://img.shields.io/badge/DOI-10.1021/acs.analchem.1c00123-blue.svg)](https://doi.org/10.1021/acs.analchem.1c00123)
[![Website](https://img.shields.io/badge/web-openspecy.org-white)](https://wincowger.shinyapps.io/OpenSpecy/)
[![Gitter](https://badges.gitter.im/Open-Specy/community.svg)](https://app.gitter.im/#/room/#Open-Specy_community:gitter.im)
<!-- badges: end -->

Raman and (FT)IR spectral analysis tool for plastic particles and other 
environmental samples (Cowger et al. 2021, doi: 
[10.1021/acs.analchem.1c00123](https://doi.org/10.1021/acs.analchem.1c00123)).
With `read_any()`, Open Specy provides a single function for reading individual,
batch, or map spectral data files like .asp, .csv, .jdx, .spc, .spa, .0, and
.zip. `process_spec()` simplifies preprocessing spectra, including smoothing,
baseline correction, range restriction and flattening, intensity conversions,
wavenumber alignment, and min-max normalization. 
Spectra can be identified in batch using an onboard reference library
(Cowger et al. 2020, doi: [10.1177/0003702820929064](https://doi.org/10.1177/0003702820929064))
using `match_spec()`. A Shiny app is available via `run_app()`
or online at [https://openanalysis.org/openspecy/](https://openanalysis.org/openspecy/).

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
devtools::install_github("wincowgerDEV/OpenSpecy-package")
```

## Getting started
```r
library(OpenSpecy)
run_app()
```

See
[package vignette](http://wincowger.com/OpenSpecy-package/articles/sop.html)
for a detailed standard operating procedure.

## Workflow

```r
# Fetch current spectral library from https://osf.io/x7dpz/
get_lib("derivative")

# Load library into global environment
spec_lib <- load_lib("derivative")

# Read sample spectrum
raman_hdpe <- read_extdata("raman_hdpe.csv") |> 
                read_any()

# Process the spectra and conform it to the library format. 
raman_adj <- raman_hdpe |>
  process_spec(conform_spec_args = list(range = spec_lib$wavenumbers), 
               make_rel = T)

cors <- cor_spec(raman_adj, spec_lib) |>
    ident_spec(x = raman_adj, y = spec_lib)
    
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
