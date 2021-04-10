## Test environments

* local Linux/5.10.23-1-MANJARO, R-4.0.4
* ubuntu 20.04 (via GitHub Actions), R-release
* ubuntu 20.04 (via GitHub Actions), R-devel
* windows latest (via GitHub Actions), R-release


## R CMD check results

0 errors | 0 warnings | 0 note

* Days since last update: 1


## Reviewer comments

Dear maintainer,

Please see the problems shown on
<https://cran.r-project.org/web/checks/check_results_OpenSpecy.html>.

Please correct before 2021-04-23 to safely retain your package on CRAN.

It seems we need to remind you of the CRAN policy:

'Packages which use Internet resources should fail gracefully with an informative message
if the resource is not available or has changed (and not give a check warning nor error).'

This needs correction whether or not the resource recovers.

The CRAN Team

> Sorry for the inconvenience. We reviewed CRAN's policy on source packages once
> more and fixed our checking routines accordingly.
