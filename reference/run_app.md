# Run Open Specy app

This wrapper function starts the graphical user interface of Open Specy.

## Usage

``` r
run_app(
  path = "system",
  log = TRUE,
  ref = NULL,
  check_local = TRUE,
  test_mode = FALSE,
  launch.browser = getOption("shiny.launch.browser", interactive()),
  ...
)
```

## Arguments

- path:

  Shiny app directory, or `"system"` to launch the bundled app.

- log:

  logical; enables/disables Shiny logging to
  [`tempdir()`](https://rdrr.io/r/base/tempfile.html).

- ref:

  retained for compatibility with older releases; ignored because the
  app is bundled with the package.

- check_local:

  retained for compatibility with older releases; ignored because
  `path = "system"` always uses the bundled app.

- test_mode:

  logical; for internal testing only.

- launch.browser:

  option for
  [`shiny::runApp()`](https://rdrr.io/pkg/shiny/man/runApp.html).

- ...:

  arguments passed to
  [`shiny::runApp()`](https://rdrr.io/pkg/shiny/man/runApp.html).

## Value

This function normally does not return any value, see
[`shiny::runApp()`](https://rdrr.io/pkg/shiny/man/runApp.html). In
`test_mode`, it invisibly returns the resolved app path.

## Details

By default, `run_app()` launches the Shiny app bundled with the
installed OpenSpecy package at
`system.file("shiny", package = "OpenSpecy")`. Historical GitHub
download support has been removed so package installs use the same app
files offline. Set `path` to an explicit app directory when testing a
local Shiny app during development.

## See also

[`shiny::runApp()`](https://rdrr.io/pkg/shiny/man/runApp.html)

## Author

Win Cowger, Zacharias Steinmetz, Garth Covernton

## Examples

``` r
if (FALSE) { # \dontrun{
run_app()
} # }
```
