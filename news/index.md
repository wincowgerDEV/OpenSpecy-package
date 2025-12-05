# Changelog

## OpenSpecy 1.7.0

- Improved run_app functionality to allow for version control.
- Faster ENVI file reading.
- Add area under band calculation.

## OpenSpecy 1.5.0

### Major

- Update to vignettes for new functionality.
- Improved plots
- Improved tests for Open Specy format.
- Improved reading of csv files.
- Improved reading of spa files.
- Extended options for library version downloads.
- Simpler function calling
- Extended baseline fitting options.

## OpenSpecy 1.3.0

### Major

- added 2 new libraries a nobaseline and derivative version of medioid
  and model
- Created new function for spatial smooth without reading envi files
- Allow adj_intens to work on vectors or Open Specy objects

### Minor

- fixed bug with mac reading libraries

## OpenSpecy 1.2.0

CRAN release: 2024-09-14

### Potentially Breaking

- Removed share data options in all functions. They just weren’t useful
  to users at all and were more of an administrative thing. Keeping them
  forced us to be incompatible with webR.

### Major

- added support for siMPle files.
- added support for xyz files.
- added support for img files.
- improved interactive plot popups.
- changed how libraries are downloaded to avoid osfr pacakage.
- increased support for options when collapsing maps.
- avoid forcing min-max relative plots in interactive mode.
- create static map option.

## OpenSpecy 1.1.0

CRAN release: 2024-06-13

### Minor Improvements

- updated links

## OpenSpecy 1.0.9

### Minor Improvements

- more closing and flexibility options

## OpenSpecy 1.0.8

CRAN release: 2024-03-14

### Minor Improvements

- updated `manage_na`, `spec_res`, `read_any` for easier flow with the
  app

## OpenSpecy 1.0.7

CRAN release: 2024-03-11

### Minor Improvements

- Modified `manage_na.R`
- Added to NAMESPACE

## OpenSpecy 1.0.6

CRAN release: 2023-11-25

### Minor Improvements

- Add attributes to `OpenSpecy` objects
- More flexible
  [`sig_noise()`](https://raw.githack.com/wincowgerDEV/OpenSpecy-package/main/docs/index.html/reference/sig_noise.md)
- Simpler matching

## OpenSpecy 1.0.5

CRAN release: 2023-10-31

### Minor Improvements

- Support .tsv files

### Bug Fixes

- Flip xy coordinates in ENVI files

## OpenSpecy 1.0.4

CRAN release: 2023-10-02

### Minor Improvements

- More contributors
- `showlegend` argument for interactive plots

### Bug Fixes

- Fixes a fatal error in
  [`match_spec()`](https://raw.githack.com/wincowgerDEV/OpenSpecy-package/main/docs/index.html/reference/match_spec.md)
  probably causing incorrect identifications

## OpenSpecy 1.0.3

CRAN release: 2023-09-13

### Minor Improvements

- Simplify
  [`check_OpenSpecy()`](https://raw.githack.com/wincowgerDEV/OpenSpecy-package/main/docs/index.html/reference/as_OpenSpecy.md)
- Improve unit tests
- Improve interactive plots

## OpenSpecy 1.0.2

CRAN release: 2023-09-05

### Bug Fixes

- Set data.table threads to 2 for (CRAN) checks

## OpenSpecy 1.0.1

### Bug Fixes

- Fixed spelling mistakes
- Reduced example and test run times for CRAN

## OpenSpecy 1.0.0

### New Features

- Complete package, app, and SOP overhaul!
- The Shiny app has been outsourced to an own GitHub repository:
  <https://github.com/wincowgerDEV/OpenSpecy-shiny>
- Spectra are now stored in dedicated `OpenSpecy` objects, which can be
  managed with a set of new functions including
  [`c_spec()`](https://raw.githack.com/wincowgerDEV/OpenSpecy-package/main/docs/index.html/reference/manage_spec.md)
  for concatenating spectra or converting them back to tables
- Various functions have been renamed and improved, for instance, to
  facilitate reading (and writing) spectral files
- New functions include
  [`def_features()`](https://raw.githack.com/wincowgerDEV/OpenSpecy-package/main/docs/index.html/reference/def_features.md)
  to identify microplastics in spectral maps and
  [`ai_classify()`](https://raw.githack.com/wincowgerDEV/OpenSpecy-package/main/docs/index.html/reference/match_spec.md)
  to use AI for matching/identifying spectra

### Minor Improvements

- Added pkgdown documentation
- Added code coverage tests

## OpenSpecy 0.9.5

CRAN release: 2022-07-06

### Bug Fixes

- Fixed outdated links and redirects

## OpenSpecy 0.9.4

### Minor Improvements

- UI improvements
- Gitter support

### Bug Fixes

- Fixed invalid regex failing CRAN checks

## OpenSpecy 0.9.3

CRAN release: 2021-10-13

### Minor Improvements

- Better error handling for .csv formats
- Add funders and goals
- Updated package citation
- CI testing for Mac

### Bug Fixes

- Fixed testthat routines occasionally failing CRAN checks

## OpenSpecy 0.9.2

CRAN release: 2021-05-20

### New Features

- Manual baseline corrections
- Citable technical note

### Minor Improvements

- More generic .spa file reading
- Added funding

### Bug Fixes

- UI improvements

## OpenSpecy 0.9.1

CRAN release: 2021-04-11

### Bug Fixes

- Checks fail gracefully if api.osf.io is not reachable
- Adjust UI selectors to comply with inverse axis and not exceed ranges

## OpenSpecy 0.9.0

CRAN release: 2021-04-09

### New Features

- UI overhaul
- Give more control to the user when starting via
  [`run_app()`](https://raw.githack.com/wincowgerDEV/OpenSpecy-package/main/docs/index.html/reference/run_app.md)

### Minor Improvements

- Reverse spectral axes to comply with most wavenumber scales
- Let users select metadata license
- Improved data sharing and logging capabilities
- Google Analytics removed

### Bug Fixes

- Use tempdir for unit tests and examples

## OpenSpecy 0.8.2

CRAN release: 2021-03-31

### Minor Improvements

- Compliance with CRAN style guide
- More references with DOIs
- Better error/warning messages during Shiny file input

### Bug Fixes

- Fixed bug with Shiny reactive values

## OpenSpecy 0.8.1

### Bug Fixes

- Fix redirecting URLs

## OpenSpecy 0.8.0

### New Features

- Use external Open Specy libraries from OSF
- [`read_asp()`](https://raw.githack.com/wincowgerDEV/OpenSpecy-package/main/docs/index.html/reference/read_ext.md)
  for reading Agilent .asp files
- GUI overhaul
- Comprehensive package vignette and function documentation
- Unit testing for main functions

### Minor Improvements

- Better error handling
- Stripped down dependencies

## OpenSpecy 0.7.0

- Transferred code base from openspecy.org to this R package
