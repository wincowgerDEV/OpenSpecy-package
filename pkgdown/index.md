# OpenSpecy

<div class="openspecy-app-shell" id="openspecy-app-shell" data-openspecy-embed>
<div class="openspecy-app-toolbar">
<div class="openspecy-app-heading">
<strong>OpenSpecy web app</strong>
<span id="openspecy-app-status" role="status" aria-live="polite">Loading application...</span>
</div>
<div class="openspecy-app-actions">
<a class="btn btn-sm btn-outline-secondary" href="app/" target="_blank" rel="noopener">Open separately</a>
<button class="btn btn-sm btn-primary" id="openspecy-fullscreen" type="button" aria-controls="openspecy-app-frame" aria-pressed="false" disabled>Full screen</button>
</div>
</div>
<div class="openspecy-app-viewport">
<div class="openspecy-app-loading" data-openspecy-loading>
<div class="openspecy-app-loading-content">
<p class="openspecy-app-loading-title">Loading OpenSpecy</p>
<div class="openspecy-app-progress" role="progressbar" aria-label="Loading the OpenSpecy web application" aria-valuetext="Loading">
<span></span>
</div>
<p>WebR is starting in your browser. You can keep exploring the documentation while it loads.</p>
</div>
</div>
<iframe id="openspecy-app-frame" title="OpenSpecy spectral analysis application" src="app/" loading="eager"></iframe>
</div>
</div>

<noscript>The embedded app requires JavaScript. <a href="app/">Open the OpenSpecy web app directly.</a></noscript>

The browser app is built from the same bundled Shiny source as `run_app()`.
Each hosted build includes the WebAssembly library image produced for the
matching OpenSpecy commit and its pinned dependency closure. To keep the
download practical, the browser app exposes the medoid and multinomial model
libraries; the local app continues to support the full libraries.

Analyze, Process, Identify, and Share Raman and (FT)IR Spectra

<!-- badges: start -->
[![CRAN version](https://www.r-pkg.org/badges/version/OpenSpecy)](https://CRAN.R-project.org/package=OpenSpecy) 
[![Project Status](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![R-CMD-check](https://github.com/wincowgerDEV/OpenSpecy-package/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/wincowgerDEV/OpenSpecy-package/actions/workflows/R-CMD-check.yaml)
[![Codecov test coverage](https://codecov.io/gh/wincowgerDEV/OpenSpecy-package/branch/main/graph/badge.svg)](https://app.codecov.io/gh/wincowgerDEV/OpenSpecy-package?branch=main)
[![License: CC BY 4.0](https://img.shields.io/badge/license-CC%20BY%204.0-lightgrey.svg)](https://creativecommons.org/licenses/by/4.0/)
[![DOI](https://img.shields.io/badge/DOI-10.1021/acs.analchem.5c00962-blue.svg)](https://doi.org/10.1021/acs.analchem.5c00962)
[![Website](https://img.shields.io/badge/web-openspecy.org-white)](https://wincowgerdev.github.io/OpenSpecy-package/)
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

## Community and help

Join the hundreds of researchers around the world who are part of the Open
Specy community by analyzing, sharing, processing, and identifying Raman and
infrared spectra. Open Specy is free and open source thanks to its community
and partners.

- [Follow Win Cowger on LinkedIn](https://www.linkedin.com/in/win-cowger/) for
  project updates.
- [Watch OpenSpecy development on GitHub](https://github.com/wincowgerDEV/OpenSpecy-package/subscription),
  [report an issue](https://github.com/wincowgerDEV/OpenSpecy-package/issues),
  or request a feature.
- Email [wincowger@gmail.com](mailto:wincowger@gmail.com?subject=Open%20Specy%20mailing%20list)
  to join the Open Specy mailing list.
- [Watch the Open Specy introduction](https://www.youtube.com/watch?v=3RKufDxzriE),
  explore the [tutorial playlist](https://www.youtube.com/playlist?list=PLqdH8O1nalYa4a8JXQ6GbNsH3YQV_aY7g),
  or follow the [standard operating procedure](articles/sop.html).
- The [classic OpenSpecy app](https://wincowger.shinyapps.io/openspecy-classic)
  remains available for users who need the earlier interface.

### Spectroscopy resources

- [siMPle](https://simple-plastics.eu/) for microplastic FTIR identification.
- [GEPARD](https://gitlab.ipfdd.de/GEPARD/gepard) for particle-based Raman and
  FTIR microplastic analysis.
- [MolView](https://molview.org/) for chemical modeling and spectral queries.
- [NIST Chemistry WebBook](https://webbook.nist.gov/) for chemical and
  spectroscopy reference data.
- [Thermo Fisher molecular spectroscopy learning center](https://www.thermofisher.com/us/en/home/industrial/spectroscopy-elemental-isotope-analysis/spectroscopy-elemental-isotope-analysis-learning-center/molecular-spectroscopy-information.html).
- [Florida State University optical microscopy primer](https://micro.magnet.fsu.edu/primer/).
- [Spectragryph](https://www.effemm2.de/spectragryph/index.html) for desktop
  spectral analysis and reference-database links.

## Partner with us

Help us make open spectroscopy tools and data more useful and accessible.

- [Donate to Open Specy](https://www.paypal.com/cgi-bin/webscr?cmd=_donations&business=wincowger@gmail.com&lc=US&item_name=Donation+to+Open+Specy&no_note=0&cn=&currency_code=USD&bn=PP-DonationsBF:btn_donateCC_LG.gif:NonHosted).
- [Buy Open Specy merchandise](https://shop.spreadshirt.com/openspecy/all).
- Review the [community contribution guidelines](https://docs.google.com/document/d/1SaFgAYKsLbMSYdJClR5s42TyGmPRWihLQcf5zun_yfo/edit?usp=sharing)
  to volunteer time or expertise.
- Contribute spectra through the [Community Data Warehouse](https://osf.io/rjg3c/).
  Shared community data use the Creative Commons Attribution 4.0 license.

### Partners and contributors

We gratefully recognize the organizations and people whose financial or in-kind
support has sustained Open Specy.

**Monetary partners**

- **Thriving ($10,000–$100,000):** Moore Institute for Plastic Pollution
  Research; Helmholtz Information & Data Science Academy; National Renewable
  Energy Laboratory; McPZ Foundation.
- **Maintaining ($1,000–$10,000):** University of California, Riverside;
  National Science Foundation; Alfred Wegener Institute; Hawai'i Pacific
  University; National Institute of Standards and Technology; University of
  Toronto; University of Koblenz-Landau; Thermo Fisher Scientific.
- **Supporting ($100–$1,000):** Jennifer Gadd.
- **Saving (under $100):** Anne Jefferson; Heather Szafranski; Gwendolyn
  Lattin; Collin Weber; Gregory Gearhart; Anika Ballent; Shelly Moore; Susanne
  Brander (Oregon State University); Jeremy Conkle (Texas A&M University–Corpus
  Christi).

**In-kind partners**

- **Thriving ($10,000–$100,000):** Win Cowger; Zacharias Steinmetz.
- **Maintaining ($1,000–$10,000):** Garth Covernton; Jamie Leonard; Shelly
  Moore; Rachel Kozloski; Katherine Lasdin; Aleksandra Karapetrova; Laura
  Markley; Walter Yu; Walter Waldman; Vesna Teofilovic; Monica Arienzo; Mary Fey
  Long Norris; Cristiane Vidal; Scott Coffin; Charles Moore; Aline Carvalho;
  Shreyas Patankar; Andrea Faltynkova; Sebastian Primpke; Andrew Gray; Chelsea
  Rochman; Orestis Herodotu; Hannah De Frond; Keenan Munno; Hannah Hapich;
  Jennifer Lynch.
- **Supporting ($100–$1,000):** Alexandre Dehaut; Gabriel Erni Cassola.

## Contract services

Our experienced spectroscopists are available for contract work including:

- adding new OpenSpecy features;
- creating spectroscopy software;
- microplastic sample analysis;
- spectral identification;
- study design; and
- related custom spectroscopy projects.

Email [wincowger@gmail.com](mailto:wincowger@gmail.com?subject=Open%20Specy%20contract%20services)
to discuss a project.

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

See [package vignette](https://wincowgerdev.github.io/OpenSpecy-package/articles/sop.html)
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

## Related Packages
### Open Specy on Python

Kris Heath created a Open Specy python package! 
https://pypi.org/project/openspi/

## Citations

Cowger W, Steinmetz Z, Gray A, Munno K, Lynch J, Hapich H, Primpke S,
De Frond H, Rochman C, Herodotou O (2021). â€œMicroplastic Spectral Classification
Needs an Open Source Community: Open Specy to the Rescue!â€
*Analytical Chemistry*, **93**(21), 7543â€“7548. doi:
[10.1021/acs.analchem.1c00123](https://doi.org/10.1021/acs.analchem.1c00123).

Cowger W, Steinmetz Z, Leong N, Faltynkova A, Sherrod H (2024). â€œOpenSpecy: Analyze,
Process, Identify, and Share Raman and (FT)IR Spectra.â€ *R package*, **1.0.8**.
[https://github.com/wincowgerDEV/OpenSpecy-package](https://github.com/wincowgerDEV/OpenSpecy-package).
