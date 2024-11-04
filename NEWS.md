# OpenSpecy 1.3.0

## Major
- added 2 new libraries a nobaseline and derivative version of medioid and model
- Created new function for spatial smooth without reading envi files
- Allow adj_intens to work on vectors or Open Specy objects

## Minor
- fixed bug with mac reading libraries

# OpenSpecy 1.2.0

## Potentially Breaking

- Removed share data options in all functions. They just weren't useful to users at all and were more of an administrative thing. Keeping them forced us to be incompatible with webR. 

## Major

- added support for siMPle files. 
- added support for xyz files. 
- added support for img files. 
- improved interactive plot popups. 
- changed how libraries are downloaded to avoid osfr pacakage. 
- increased support for options when collapsing maps. 
- avoid forcing min-max relative plots in interactive mode. 
- create static map option. 


# OpenSpecy 1.1.0

## Minor Improvements

- updated links


# OpenSpecy 1.0.9

## Minor Improvements

- more closing and flexibility options


# OpenSpecy 1.0.8

## Minor Improvements

- updated `manage_na`, `spec_res`, `read_any` for easier flow with the app


# OpenSpecy 1.0.7

## Minor Improvements

- Modified `manage_na.R`
- Added to NAMESPACE


# OpenSpecy 1.0.6

## Minor Improvements

- Add attributes to `OpenSpecy` objects
- More flexible `sig_noise()`
- Simpler matching


# OpenSpecy 1.0.5

## Minor Improvements

- Support .tsv files

## Bug Fixes

- Flip xy coordinates in ENVI files


# OpenSpecy 1.0.4

## Minor Improvements

- More contributors
- `showlegend` argument for interactive plots

## Bug Fixes

- Fixes a fatal error in `match_spec()` probably causing incorrect
  identifications


# OpenSpecy 1.0.3

## Minor Improvements

- Simplify `check_OpenSpecy()`
- Improve unit tests
- Improve interactive plots


# OpenSpecy 1.0.2

## Bug Fixes

- Set data.table threads to 2 for (CRAN) checks


# OpenSpecy 1.0.1

## Bug Fixes

- Fixed spelling mistakes
- Reduced example and test run times for CRAN


# OpenSpecy 1.0.0

## New Features

- Complete package, app, and SOP overhaul!
- The Shiny app has been outsourced to an own GitHub repository:
  https://github.com/wincowgerDEV/OpenSpecy-shiny
- Spectra are now stored in dedicated `OpenSpecy` objects, which can be managed
  with a set of new functions including `c_spec()` for concatenating spectra or
  converting them back to tables
- Various functions have been renamed and improved, for instance, to facilitate
  reading (and writing) spectral files
- New functions include `def_features()` to identify microplastics in spectral
  maps and `ai_classify()` to use AI for matching/identifying spectra

## Minor Improvements

- Added pkgdown documentation
- Added code coverage tests


# OpenSpecy 0.9.5

## Bug Fixes

- Fixed outdated links and redirects


# OpenSpecy 0.9.4

## Minor Improvements

- UI improvements
- Gitter support

## Bug Fixes

- Fixed invalid regex failing CRAN checks 


# OpenSpecy 0.9.3

## Minor Improvements

- Better error handling for .csv formats
- Add funders and goals
- Updated package citation
- CI testing for Mac

## Bug Fixes

- Fixed testthat routines occasionally failing CRAN checks


# OpenSpecy 0.9.2

## New Features

- Manual baseline corrections
- Citable technical note

## Minor Improvements

- More generic .spa file reading
- Added funding

## Bug Fixes

- UI improvements


# OpenSpecy 0.9.1

## Bug Fixes

- Checks fail gracefully if api.osf.io is not reachable
- Adjust UI selectors to comply with inverse axis and not exceed ranges


# OpenSpecy 0.9.0

## New Features

- UI overhaul
- Give more control to the user when starting via `run_app()`

## Minor Improvements

- Reverse spectral axes to comply with most wavenumber scales
- Let users select metadata license
- Improved data sharing and logging capabilities
- Google Analytics removed

## Bug Fixes

- Use tempdir for unit tests and examples


# OpenSpecy 0.8.2

## Minor Improvements

- Compliance with CRAN style guide
- More references with DOIs
- Better error/warning messages during Shiny file input

## Bug Fixes

- Fixed bug with Shiny reactive values


# OpenSpecy 0.8.1

## Bug Fixes

- Fix redirecting URLs


# OpenSpecy 0.8.0

## New Features

- Use external Open Specy libraries from OSF
- `read_asp()` for reading Agilent .asp files
- GUI overhaul
- Comprehensive package vignette and function documentation
- Unit testing for main functions

## Minor Improvements

- Better error handling
- Stripped down dependencies


# OpenSpecy 0.7.0

- Transferred code base from openspecy.org to this R package
