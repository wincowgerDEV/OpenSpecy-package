# OpenSpecy

Analyze, Process, Identify, and Share Raman and (FT)IR Spectra

<!-- badges: start -->
[![CRAN version](https://www.r-pkg.org/badges/version/OpenSpecy)](https://CRAN.R-project.org/package=OpenSpecy) 
[![Project Status](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![R-CMD-check](https://github.com/wincowgerDEV/OpenSpecy-package/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/wincowgerDEV/OpenSpecy-package/actions/workflows/R-CMD-check.yaml)
[![Codecov test coverage](https://codecov.io/gh/wincowgerDEV/OpenSpecy-package/branch/main/graph/badge.svg)](https://app.codecov.io/gh/wincowgerDEV/OpenSpecy-package?branch=main)
[![License: CC BY 4.0](https://img.shields.io/badge/license-CC%20BY%204.0-lightgrey.svg)](https://creativecommons.org/licenses/by/4.0/)
[![DOI](https://img.shields.io/badge/DOI-10.1021/acs.analchem.5c00962-blue.svg)](https://doi.org/10.1021/acs.analchem.5c00962)
[![Website](https://img.shields.io/badge/web-OpenSpecy%20app-white)](https://www.openanalysis.org/OpenSpecyV2/)
<!-- badges: end -->

Raman and (FT)IR spectral analysis tool for 
environmental samples with a special focus on microplastics (Cowger et al. 2025, doi: 
[10.1021/acs.analchem.5c00962](https://doi.org/10.1021/acs.analchem.5c00962)).
With `read_any()`, Open Specy provides a single function for reading individual,
batch, or map spectral data files like .asp, .csv, .jdx, .spc, .spa, .0, and
.zip. `process_spec()` simplifies processing spectra, including smoothing,
baseline correction, range restriction and flattening, intensity conversions,
wavenumber alignment, and min-max normalization. 
Spectra can be identified in batch using an onboard reference library
using `match_spec()`. A bundled Shiny app is available via `run_app()`
or directly on this website.

## Use OpenSpecy online

Use the hosted browser app on the
[OpenSpecy website](https://www.openanalysis.org/OpenSpecyV2/). The
local app remains available through `run_app()`.

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

## Simple workflow for single spectral identification

See [package vignette](https://www.openanalysis.org/OpenSpecyV2/pkgdown/articles/sop.html)
for a detailed standard operating procedure.

```r
# Fetch the current spectral library from the Open Specy AWS distribution
get_lib("derivative")

# Load library into global environment
spec_lib <- load_lib("derivative")

# Read sample spectrum
raman_hdpe <- read_extdata("raman_hdpe.csv") |> 
  read_any()

# Look at the spectrum
plotly_spec(raman_hdpe)

# Process the spectra and conform it to the library format
raman_proc <- raman_hdpe |>
  process_spec(conform_spec_args = list(range = spec_lib$wavenumber),
               smooth_intens = TRUE, make_rel = TRUE)

# Compare raw and processed spectra
plotly_spec(raman_hdpe, raman_proc)

top_matches <- match_spec(raman_proc, library = spec_lib, na.rm = TRUE, top_n = 5,
                          add_library_metadata = "sample_name",
                          add_object_metadata = "col_id")

# Print the top 5 results with relevant metadata
top_matches[, c("object_id", "library_id", "match_val", "SpectrumType",
                "SpectrumIdentity")]

# Get all metadata for the matches
get_metadata(spec_lib, logic = top_matches$library_id)
```

## Related Packages
### Open Specy on Python

Kris Heath created a Open Specy python package! 
https://pypi.org/project/openspi/

## Citations

Cowger W, Steinmetz Z, Gray A, Munno K, Lynch J, Hapich H, Primpke S,
De Frond H, Rochman C, Herodotou O (2021). "Microplastic Spectral Classification
Needs an Open Source Community: Open Specy to the Rescue!"
*Analytical Chemistry*, **93**(21), 7543-7548. doi:
[10.1021/acs.analchem.1c00123](https://doi.org/10.1021/acs.analchem.1c00123).

Cowger W et al. (2025). "Open Specy 1.0: Automated (Hyper)spectroscopy
for Microplastics." *Analytical Chemistry*, **97**(32), 17345-17356. doi:
[10.1021/acs.analchem.5c00962](https://doi.org/10.1021/acs.analchem.5c00962).
