# Open Specy 1.0

Analyze, Process, Identify, and Share Raman and (FT)IR Spectra

Raman and (FT)IR spectral analysis tool for environmental samples with a
special focus on microplastics (Cowger et al. 2025, doi:
[10.1021/acs.analchem.5c00962](https://doi.org/10.1021/acs.analchem.5c00962)).
With
[`read_any()`](https://raw.githack.com/wincowgerDEV/OpenSpecy-package/main/docs/index.html/reference/read_multi.md),
Open Specy provides a single function for reading individual, batch, or
map spectral data files like .asp, .csv, .jdx, .spc, .spa, .0, and .zip.
[`process_spec()`](https://raw.githack.com/wincowgerDEV/OpenSpecy-package/main/docs/index.html/reference/process_spec.md)
simplifies processing spectra, including smoothing, baseline correction,
range restriction and flattening, intensity conversions, wavenumber
alignment, and min-max normalization. Spectra can be identified in batch
using an onboard reference library using
[`match_spec()`](https://raw.githack.com/wincowgerDEV/OpenSpecy-package/main/docs/index.html/reference/match_spec.md).
A Shiny app is available via
[`run_app()`](https://raw.githack.com/wincowgerDEV/OpenSpecy-package/main/docs/index.html/reference/run_app.md)
or online at <https://www.openanalysis.org/openspecy/>.

## Installation

**OpenSpecy** is available from CRAN and GitHub.

### Install from CRAN (stable version)

You can install the latest release of **OpenSpecy** from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("OpenSpecy")
```

### Install from GitHub (development version)

To install the development version of this package, paste the following
code into your R console (requires **devtools**):

``` r
if (!require(devtools)) install.packages("devtools")
devtools::install_github("wincowgerDEV/OpenSpecy-package")
```

### Install on Web Assemply through webr (experimental), you can test here: <https://webr.r-wasm.org/latest/>

``` r
library(caTools)
library(data.table)
library(jsonlite)
library(yaml)
library(hyperSpec)
library(mmand)
library(plotly)
library(digest)
library(signal)
library(glmnet)
library(jpeg)
library(shiny)
webr::install("OpenSpecy", repos = "https://wincowger.com/OpenSpecy-package/")
library(OpenSpecy)
```

## Getting started

``` r
library(OpenSpecy)
run_app()
```

## Simple workflow for single spectral identification

See [package
vignette](https://raw.githack.com/wincowgerDEV/OpenSpecy-package/main/docs/articles/sop.html)
for a detailed standard operating procedure.

``` r
# Fetch current spectral library from https://osf.io/x7dpz/
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
  process_spec(conform_spec_args = list(range = spec_lib$wavenumbers), 
               smooth_intens = T, make_rel = T)

# Compare raw and processed spectra
plotly_spec(raman_hdpe, raman_proc)

top_matches <- match_spec(raman_proc, library = spec_lib, na.rm = T, top_n = 5,
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
<https://pypi.org/project/openspi/>

## Citations

Cowger W, Steinmetz Z, Gray A, Munno K, Lynch J, Hapich H, Primpke S, De
Frond H, Rochman C, Herodotou O (2021). “Microplastic Spectral
Classification Needs an Open Source Community: Open Specy to the
Rescue!” *Analytical Chemistry*, **93**(21), 7543–7548. doi:
[10.1021/acs.analchem.1c00123](https://doi.org/10.1021/acs.analchem.1c00123).

Cowger W, Steinmetz Z, Leong N, Faltynkova A, Sherrod H (2024).
“OpenSpecy: Analyze, Process, Identify, and Share Raman and (FT)IR
Spectra.” *R package*, **1.0.8**.
<https://github.com/wincowgerDEV/OpenSpecy-package>.
