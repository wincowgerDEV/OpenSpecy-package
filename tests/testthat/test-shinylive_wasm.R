read_wasm_manifest_lines <- function(path) {
  x <- readLines(path, warn = FALSE)
  x <- trimws(sub("#.*$", "", x))
  x[nzchar(x)]
}

wasm_manifest_path <- function(file) {
  installed <- system.file("shiny", "wasm", file, package = "OpenSpecy")
  if (nzchar(installed)) return(installed)
  test_path("..", "..", "inst", "shiny", "wasm", file)
}

test_that("Shinylive wasm package roots include app runtime packages", {
  roots <- read_wasm_manifest_lines(wasm_manifest_path("app-package-roots.txt"))

  expect_true("local::." %in% roots)
  expect_true(all(.openspecy_app_packages() %in% roots))
  expect_true(all(c(
    "shiny", "plotly", "data.table", "jsonlite", "OpenSpecy"
  ) %in% c(roots, "OpenSpecy")))
  expect_true(all(c("curl", "scales") %in% .openspecy_app_packages()))
})

test_that("Shinylive wasm library allow-list is intentionally small", {
  library_types <- read_wasm_manifest_lines(wasm_manifest_path("library-types.txt"))

  expect_setequal(
    library_types,
    c("medoid_derivative", "medoid_nobaseline",
      "model_derivative", "model_nobaseline")
  )
  expect_false(any(c("derivative", "nobaseline", "raw") %in% library_types))
})

test_that("bundled Shiny app exposes medoid/model only in wasm mode", {
  missing <- .openspecy_app_packages()[
    !vapply(.openspecy_app_packages(), requireNamespace, logical(1),
            quietly = TRUE)
  ]
  skip_if(length(missing), paste(
    "Missing Shiny app packages:",
    paste(missing, collapse = ", ")
  ))

  app_path <- run_app(test_mode = TRUE)
  env <- new.env(parent = globalenv())
  old_wd <- getwd()
  old_opt <- getOption("openspecy.shiny.wasm")
  old_libs <- getOption("openspecy.shiny.wasm.libraries")
  old_env <- Sys.getenv("OPENSPECY_SHINY_WASM", unset = NA)
  on.exit(setwd(old_wd), add = TRUE)
  on.exit({
    options(openspecy.shiny.wasm = old_opt)
    options(openspecy.shiny.wasm.libraries = old_libs)
    if (is.na(old_env)) {
      Sys.unsetenv("OPENSPECY_SHINY_WASM")
    } else {
      Sys.setenv(OPENSPECY_SHINY_WASM = old_env)
    }
  }, add = TRUE)

  options(openspecy.shiny.wasm = FALSE)
  Sys.unsetenv("OPENSPECY_SHINY_WASM")
  setwd(app_path)
  sys.source(file.path(app_path, "global.R"), envir = env)

  options(openspecy.shiny.wasm = FALSE)
  expect_true("full" %in% unname(env$app_library_type_choices()))

  options(openspecy.shiny.wasm = TRUE)
  expect_setequal(unname(env$app_library_type_choices()),
                  c("medoid", "model"))
  expect_error(env$app_validate_library_type("derivative"),
               "only includes medoid and model")
  expect_error(env$app_validate_library_type("medoid_derivative"), NA)
})
