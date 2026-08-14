# OpenSpecy

Analyze, Process, Identify, and Share Raman and (FT)IR Spectra

<!-- badges: start -->
[![CRAN version](https://www.r-pkg.org/badges/version/OpenSpecy)](https://CRAN.R-project.org/package=OpenSpecy) 
[![Project Status](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![R-CMD-check](https://github.com/wincowgerDEV/OpenSpecy-package/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/wincowgerDEV/OpenSpecy-package/actions/workflows/R-CMD-check.yaml)
[![Codecov test coverage](https://codecov.io/gh/wincowgerDEV/OpenSpecy-package/branch/main/graph/badge.svg)](https://app.codecov.io/gh/wincowgerDEV/OpenSpecy-package?branch=main)
[![License: CC BY 4.0](https://img.shields.io/badge/license-CC%20BY%204.0-lightgrey.svg)](https://creativecommons.org/licenses/by/4.0/)
[![DOI](https://img.shields.io/badge/DOI-10.1021/acs.analchem.5c00962-blue.svg)](https://doi.org/10.1021/acs.analchem.5c00962)
[![Website](https://img.shields.io/badge/web-OpenSpecy%20app-white)](https://wincowgerdev.github.io/OpenSpecy-package/)
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
[OpenSpecy website](https://wincowgerdev.github.io/OpenSpecy-package/). The
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

See [package vignette](https://wincowgerdev.github.io/OpenSpecy-package/pkgdown/articles/sop.html)
for a detailed standard operating procedure.

```r
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

## Compressed Specs workflow

`as_Specs()` can compress map or library spectra for fast approximate matching.
The default workflow fits PCA and then Hilbert-encodes the PCA scores into exact
high/low 64-bit code rows.

```r
model <- fit_specs_pca(spec_lib, n_components = 16)
library_specs <- as_Specs(spec_lib, model)
query_specs <- as_Specs(raman_proc, model,
                        limits = attr(library_specs, "hilbert_model"))

match_spec(query_specs, library_specs, top_n = 5)
decompress_spec(query_specs, index = 1)
```

## Large package-only Specs workflow (experimental)

`FileSpecs` is the local-first `Specs` subtype for hyperspectral maps that are
too large to keep in memory. `open_specs()` indexes an H5 file or an ENVI
`.hdr` plus `.dat`/`.img` pair without storing the spectral cube in the R
object. Sources are opened read-only, fingerprints identify their version, and
derived cache files live outside the source. Existing matrix-backed `Specs` and
ordinary `OpenSpecy` workflows are unchanged.

```r
cache <- file.path(tempdir(), "openspecy-map-cache")
large_map <- open_specs("path/to/map.h5", cache_dir = cache)
print(large_map)

# Region splits are lightweight views. Materialization must be explicit.
regions <- split_spec(large_map, by = "region")
one_spectrum <- decompress_spec(regions[[1]], index = 1)
small_roi <- decompress_spec(large_map, region = "Region1",
                             roi = c(10, 30, 20, 40))

# A complete one-region view can be streamed to a new float64 ENVI pair.
# Existing output members are never overwritten.
write_spec(regions[[1]], "path/to/new-region.hdr")

# The first supported whole-map pipeline finds regions automatically, streams
# S/N and exact particle means, then matches the much smaller collapsed object.
particles <- automate_particle_analysis(
  large_map, library = spec_lib,
  particle_id_strategy = "collapse", collapse_function = mean,
  spectral_smooth = FALSE, sn_threshold_min = 0.04,
  cor_threshold = 0.7,
  outputs = c("details", "summary", "processed", "particle_image",
              "particle_heatmap", "sn_histogram", "cor_histogram")
)
plot(particles, sample = "Region1", which = "sn_histogram")
```

The initial package analysis contract deliberately excludes whole-map
correlation matrices, raw-pixel matching, spectral smoothing, entropy S/N,
median/custom collapse, and PCA/K-means fitting. Use a bounded
`decompress_spec()` selection when another established `OpenSpecy` operation is
needed. Particle analysis retains the exact best match after collapse.
Requesting `particle_image` for H5 data stitches
and caches only the current region's registered mosaic tiles. These experimental
APIs remain available to package users for future large-map research.

## In-memory app workflow

The bundled and browser apps use one in-memory `OpenSpecy` workflow. Uploads
have a 10 GiB transport ceiling, but the usable dataset size also depends on
available RAM and the selected operations. The app reports estimated resident
and peak memory before expensive work and gives recovery guidance when a known
unsafe configuration is selected.

For hyperspectral maps, optional spatial smoothing happens first. Signal/noise
is calculated from that spatial-only data. Particle collapse can use connected
threshold regions, PCA plus K-means within regions, or non-spatial PCA plus
K-means groups. Correlation-threshold collapse performs one processed matching
pass, filters pixels by the selected thresholds, and reuses the resulting
material identities while collapsing. Identification retains only the selected
Top N matches per spectrum (10 by default), and the table and download share
that same compact result instead of storing a full correlation matrix.

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
