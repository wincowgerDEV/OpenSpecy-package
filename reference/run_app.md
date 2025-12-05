# Run Open Specy app

This wrapper function starts the graphical user interface of Open Specy.

## Usage

``` r
run_app(
  path = "system",
  log = TRUE,
  ref = "main",
  check_local = TRUE,
  test_mode = FALSE,
  ...
)
```

## Arguments

- path:

  to store the downloaded app files; defaults to `"system"` pointing to
  `system.file(package = "OpenSpecy")`.

- log:

  logical; enables/disables logging to
  [`tempdir()`](https://rdrr.io/r/base/tempfile.html)

- ref:

  git reference; could be a commit, tag, or branch name. Defaults to
  "main". Setting this will ensure you always use the same version of
  the shiny app no matter what system you are on.

- check_local:

  logical; when `TRUE` a previously downloaded copy of the Shiny app
  located at `path` is used instead of downloading a fresh copy from
  GitHub. The directory may contain either a single-file `app.R`
  application or a `server.R`/`ui.R` pair. Metadata about downloaded
  copies, including the originating commit hash, is stored alongside the
  app and surfaced when the local copy is reused. When a specific `ref`
  is requested, matching local copies are preferred and unmatched
  directories are ignored.

- test_mode:

  logical; for internal testing only.

- ...:

  arguments passed to
  [`runApp()`](https://rdrr.io/pkg/shiny/man/runApp.html).

## Value

This function normally does not return any value, see
[`runApp()`](https://rdrr.io/pkg/shiny/man/runApp.html).

## Details

After running this function the Open Specy GUI should open in a separate
window or in your computer browser. When downloads are required, the
function reports the GitHub commit used and preserves that information
for subsequent reuse when `check_local = TRUE`.

## See also

[`runApp()`](https://rdrr.io/pkg/shiny/man/runApp.html)

## Author

Win Cowger, Zacharias Steinmetz, Garth Covernton

## Examples

``` r
if (FALSE) { # \dontrun{
run_app()
} # }
```
