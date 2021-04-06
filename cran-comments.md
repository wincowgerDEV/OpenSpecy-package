## Test environments

* local Arch/Manjaro 21.0, R-4.0.4
* ubuntu 20.04 (via GitHub Actions), R-release
* ubuntu 20.04 (via GitHub Actions), R-devel
* windows latest (via GitHub Actions), R-release


## R CMD check results

0 errors | 0 warnings | 0 note


## Reviewer comments

Dear maintainer,

Pls see
<https://cran.r-project.org/web/checks/check_results_OpenSpecy.html>

The check problems on the Debian systems are caused by attempts to write
to the user library to which all packages get installed before checking
(and which now is remounted read-only for checking).

Having package code which is run as part of the checks and attempts to
write to the user library violates the CRAN Policy's

  Packages should not write in the user’s home filespace (including
  clipboards), nor anywhere else on the file system apart from the R
  session’s temporary directory (or during installation in the location
  pointed to by TMPDIR: and such usage should be cleaned up).

Please correct before 2021-04-16 to safely retain your package on CRAN.

-k

> Fixed. Package functions only write to the local file system if prompted by
> the user. Functions called for CI testing and examples now write to tempdir()
> which is tidied up after use.
