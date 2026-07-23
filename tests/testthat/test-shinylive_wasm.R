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

source_wasm_tool <- function(file, env) {
  path <- test_path("..", "..", "tools", "wasm", file)
  if (!file.exists(path)) {
    skip("Repository-only wasm deployment tools are not in the package tarball")
  }
  sys.source(path, envir = env)
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

test_that("action-built wasm library image is bundled with an exact pin", {
  env <- new.env(parent = globalenv())
  source_wasm_tool("bundle-wasm-library.R", env)

  tmp <- file.path(tempdir(), "OpenSpecy-testthat-wasm-bundle")
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  repo_dir <- file.path(tmp, "repo")
  image_dir <- file.path(tmp, "image")
  site_dir <- file.path(tmp, "site")
  contrib <- file.path(repo_dir, "bin", "emscripten", "contrib", "4.6")
  dir.create(contrib, recursive = TRUE, showWarnings = FALSE)
  dir.create(image_dir, recursive = TRUE, showWarnings = FALSE)

  roots <- read_wasm_manifest_lines(wasm_manifest_path("app-package-roots.txt"))
  desc <- read.dcf(test_path("..", "..", "DESCRIPTION"))[1, ]
  roots[roots == "local::."] <- desc[["Package"]]
  versions <- rep("1.0.0", length(roots))
  versions[roots == desc[["Package"]]] <- desc[["Version"]]
  imports <- rep(NA_character_, length(roots))
  imports[roots == "dplyr"] <- "webr"
  write.dcf(data.frame(Package = roots, Version = versions,
                       Imports = imports),
            file.path(contrib, "PACKAGES"))

  writeBin(as.raw(seq_len(32)), file.path(image_dir, "library.data.gz"))
  jsonlite::write_json(
    list(files = data.frame(filename = paste0("/", roots, "/DESCRIPTION")),
         gzip = TRUE),
    file.path(image_dir, "library.js.metadata"),
    auto_unbox = TRUE
  )

  pin <- paste(rep("a", 40), collapse = "")
  env$bundle_wasm_library(
    image_dir, repo_dir, site_dir, pin,
    description_file = test_path("..", "..", "DESCRIPTION"),
    package_roots_file = wasm_manifest_path("app-package-roots.txt")
  )

  metadata <- readRDS(file.path(site_dir, "shinylive", "webr", "packages",
                                "metadata.rds"))
  expect_length(metadata, 1)
  expect_identical(metadata[[1]]$version, unname(desc[["Version"]]))
  expect_match(metadata[[1]]$ref, pin, fixed = TRUE)
  expect_identical(metadata[[1]]$type, "library")
  expect_true(all(file.exists(file.path(
    site_dir, "shinylive", "webr", "packages", metadata[[1]]$name,
    vapply(metadata[[1]]$assets, `[[`, character(1), "filename")
  ))))

  manifest <- jsonlite::fromJSON(file.path(site_dir,
                                            "pinned-wasm-library.json"))
  expect_identical(manifest$package$version, unname(desc[["Version"]]))
  expect_identical(manifest$package$commit, pin)
})

test_that("wasm package resolver includes the transitive hard closure", {
  env <- new.env(parent = globalenv())
  source_wasm_tool("resolve-wasm-package-roots.R", env)

  tmp <- file.path(tempdir(), "OpenSpecy-testthat-wasm-resolver")
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  dir.create(tmp, recursive = TRUE, showWarnings = FALSE)
  roots_file <- file.path(tmp, "roots.txt")
  description_file <- file.path(tmp, "DESCRIPTION")
  writeLines(c("local::.", "Alpha"), roots_file)
  write.dcf(
    data.frame(Package = "LocalPackage", Version = "1.0.0",
               Imports = "Alpha, Matrix, methods"),
    description_file
  )
  available <- rbind(
    Alpha = c(Package = "Alpha", Version = "1.0.0", Depends = NA,
              Imports = "Beta", LinkingTo = NA),
    Beta = c(Package = "Beta", Version = "1.0.0", Depends = NA,
             Imports = "Gamma", LinkingTo = NA),
    Gamma = c(Package = "Gamma", Version = "1.0.0", Depends = NA,
              Imports = NA, LinkingTo = NA),
    Matrix = c(Package = "Matrix", Version = "1.7.4", Depends = "R",
               Imports = "methods", LinkingTo = NA)
  )

  resolved <- env$resolve_wasm_package_roots(
    roots_file,
    description_file,
    available = available,
    platform_packages = "methods"
  )
  expect_identical(resolved,
                   c("local::.", "Alpha", "Beta", "Gamma", "Matrix"))
})

test_that("wasm library bundling rejects an incomplete hard closure", {
  env <- new.env(parent = globalenv())
  source_wasm_tool("bundle-wasm-library.R", env)

  tmp <- file.path(tempdir(), "OpenSpecy-testthat-wasm-incomplete")
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  repo_dir <- file.path(tmp, "repo")
  image_dir <- file.path(tmp, "image")
  contrib <- file.path(repo_dir, "bin", "emscripten", "contrib", "4.6")
  dir.create(contrib, recursive = TRUE, showWarnings = FALSE)
  dir.create(image_dir, recursive = TRUE, showWarnings = FALSE)

  roots <- read_wasm_manifest_lines(wasm_manifest_path("app-package-roots.txt"))
  desc <- read.dcf(test_path("..", "..", "DESCRIPTION"))[1, ]
  roots[roots == "local::."] <- desc[["Package"]]
  packages <- data.frame(Package = roots, Version = "1.0.0",
                         Imports = NA_character_)
  packages$Version[packages$Package == desc[["Package"]]] <- desc[["Version"]]
  packages$Imports[packages$Package == "dplyr"] <- "Matrix"
  write.dcf(packages, file.path(contrib, "PACKAGES"))
  writeBin(as.raw(seq_len(8)), file.path(image_dir, "library.data.gz"))
  jsonlite::write_json(
    list(files = data.frame(filename = paste0("/", roots, "/DESCRIPTION"))),
    file.path(image_dir, "library.js.metadata"),
    auto_unbox = TRUE
  )

  expect_error(
    env$bundle_wasm_library(
      image_dir, repo_dir, file.path(tmp, "site"), strrep("b", 40),
      description_file = test_path("..", "..", "DESCRIPTION"),
      package_roots_file = wasm_manifest_path("app-package-roots.txt")
    ),
    "Matrix"
  )
})

test_that("assembled Pages site contains pkgdown and only the bundled app", {
  env <- new.env(parent = globalenv())
  source_wasm_tool("check-pages-site.R", env)

  tmp <- file.path(tempdir(), "OpenSpecy-testthat-pages-site")
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  dir.create(file.path(tmp, "app"), recursive = TRUE,
             showWarnings = FALSE)
  writeLines(c(
    '<meta name="generator" content="pkgdown">',
    '<div data-openspecy-embed>',
    '<iframe id="openspecy-app-frame" src="app/"></iframe>',
    '</div>'
  ),
             file.path(tmp, "index.html"))
  writeLines("runExportedApp({});", file.path(tmp, "app", "index.html"))

  expect_no_error(env$check_pages_site(tmp, max_bytes = 1024^2))
  writeLines(c(
    '<meta name="generator" content="pkgdown">',
    '<div data-openspecy-embed>',
    '<div class="sourceCode"><iframe id="openspecy-app-frame"',
    'src="app/"></iframe></div>'
  ), file.path(tmp, "index.html"))
  expect_error(env$check_pages_site(tmp, max_bytes = 1024^2),
               "rendered as a source-code block")
  writeLines(c(
    '<meta name="generator" content="pkgdown">',
    '<div data-openspecy-embed>',
    '<iframe id="openspecy-app-frame" src="app/"></iframe>',
    '</div>'
  ), file.path(tmp, "index.html"))
  dir.create(file.path(tmp, "wasm"))
  expect_error(env$check_pages_site(tmp, max_bytes = 1024^2),
               "must not contain a wasm repository")
})

test_that("only one workflow publishes the combined native Pages site", {
  workflow_dir <- test_path("..", "..", ".github", "workflows")
  if (!dir.exists(workflow_dir)) {
    skip("Repository-only workflow files are not in the package tarball")
  }

  workflow_files <- list.files(workflow_dir, pattern = "\\.ya?ml$",
                               full.names = TRUE)
  workflow_text <- unlist(lapply(workflow_files, readLines, warn = FALSE))
  shinylive <- readLines(file.path(workflow_dir, "deploy-shinylive.yml"),
                         warn = FALSE)
  wasm <- readLines(file.path(workflow_dir, "deploy-cran-repo.yml"),
                    warn = FALSE)

  expect_false(any(grepl("github-pages-deploy-action", workflow_text,
                         fixed = TRUE)))
  expect_equal(sum(grepl("actions/deploy-pages@v4", workflow_text,
                         fixed = TRUE)), 1L)
  expect_true(any(grepl("actions/configure-pages@v5", shinylive,
                        fixed = TRUE)))
  expect_true(any(grepl("actions/upload-pages-artifact@v4", shinylive,
                        fixed = TRUE)))
  expect_true(any(grepl('dest_dir = "_site"', shinylive, fixed = TRUE)))
  expect_true(any(grepl("_site/app", shinylive, fixed = TRUE)))
  expect_false(any(grepl("_site/openspecy", shinylive, fixed = TRUE)))
  expect_true(any(grepl('repository: ${{ github.repository }}', shinylive,
                        fixed = TRUE)))
  expect_false(any(grepl("wincowgerDEV/OpenSpecy-package", shinylive,
                         fixed = TRUE)))
  expect_true(any(grepl(
    "SHINYLIVE_SMOKE_URL=http://127.0.0.1:8080/ ",
    shinylive, fixed = TRUE
  )))
  expect_false(any(grepl("_site/wasm", c(shinylive, wasm), fixed = TRUE)))
  expect_true(any(grepl("path: _wasm/pinned", wasm, fixed = TRUE)))
  expect_equal(sum(grepl("pak-version: repo", shinylive, fixed = TRUE)), 1L)
  expect_equal(sum(grepl("pak-version: repo", wasm, fixed = TRUE)), 1L)
})

test_that("pkgdown homepage and Shiny app provide the embed handshake", {
  app_path <- run_app(test_mode = TRUE)
  ui_source <- readLines(file.path(app_path, "ui.R"), warn = FALSE)
  bridge_path <- file.path(app_path, "www", "parent-frame.js")

  expect_true(file.exists(bridge_path))
  expect_true(any(grepl("parent-frame.js", ui_source, fixed = TRUE)))
  bridge <- readLines(bridge_path, warn = FALSE)
  expect_true(any(grepl("shiny:idle.openspecyParent", bridge,
                        fixed = TRUE)))
  expect_true(any(grepl("window.top.postMessage", bridge, fixed = TRUE)))
  expect_true(any(grepl("openspecy:ready", bridge, fixed = TRUE)))
  expect_true(any(grepl("shiny:busy.openspecyBusy", bridge, fixed = TRUE)))
  expect_true(any(grepl("openspecy-analysis-phase", bridge, fixed = TRUE)))
  expect_false(any(grepl("shiny:value.openspecyBusy", bridge, fixed = TRUE)))
  expect_true(any(grepl("openspecy-busy-visible", bridge, fixed = TRUE)))

  readme_path <- test_path("..", "..", "README.md")
  homepage_path <- test_path("..", "..", "pkgdown", "index.md")
  pkgdown_dir <- test_path("..", "..", "pkgdown")
  if (!file.exists(readme_path) || !file.exists(homepage_path) ||
      !dir.exists(pkgdown_dir)) {
    skip("Repository-only pkgdown sources are not in the package tarball")
  }

  readme <- readLines(readme_path, warn = FALSE)
  homepage <- readLines(homepage_path, warn = FALSE)
  script <- readLines(file.path(pkgdown_dir, "extra.js"), warn = FALSE)
  css <- readLines(file.path(pkgdown_dir, "extra.css"), warn = FALSE)
  expect_false(any(grepl("data-openspecy-embed", readme, fixed = TRUE)))
  expect_false(any(grepl("openspecy-app-frame", readme, fixed = TRUE)))
  expect_true(any(grepl("data-openspecy-embed", homepage, fixed = TRUE)))
  expect_true(any(grepl('src="app/"', homepage, fixed = TRUE)))
  expect_false(any(grepl('src="openspecy/"', homepage, fixed = TRUE)))
  expect_true(all(vapply(
    c("## Community and help", "## Partner with us", "## Contract services"),
    function(heading) any(grepl(heading, homepage, fixed = TRUE)),
    logical(1)
  )))
  expect_lt(which(grepl("data-openspecy-embed", homepage,
                        fixed = TRUE))[[1]],
            which(grepl("Analyze, Process, Identify", homepage,
                        fixed = TRUE))[[1]])
  expect_false(any(grepl("requestFullscreen", script, fixed = TRUE)))
  expect_true(any(grepl("openspecy-app-fullscreen-open", script,
                        fixed = TRUE)))
  expect_true(any(grepl("DOMContentLoaded", script, fixed = TRUE)))
  expect_true(any(grepl("event.origin !== window.location.origin", script,
                        fixed = TRUE)))
  expect_true(any(grepl("openspecy-app-shell.is-fullscreen", css,
                        fixed = TRUE)))
})

test_that("bundled app has no floating wasm package installer", {
  app_path <- run_app(test_mode = TRUE)
  global_source <- readLines(file.path(app_path, "global.R"), warn = FALSE)
  server_source <- readLines(file.path(app_path, "server.R"), warn = FALSE)

  expect_false(any(grepl("webr::install", global_source, fixed = TRUE)))
  expect_false(any(grepl("install_wasm_packages", global_source,
                         fixed = TRUE)))
  expect_true(any(grepl("validate_wasm_package_version", global_source,
                        fixed = TRUE)))
  expect_false(any(grepl("curl::has_internet()", server_source,
                         fixed = TRUE)))
  expect_false(any(grepl("googletranslate|output\\$translate",
                         c(global_source, server_source))))

  prepare_path <- test_path("..", "..", "tools", "wasm",
                            "prepare-shinylive-app.R")
  if (file.exists(prepare_path)) {
    prepare_source <- readLines(prepare_path, warn = FALSE)
    expect_true(any(grepl("openspecy.shiny.wasm.artifact", prepare_source,
                          fixed = TRUE)))
    expect_false(any(grepl("openspecy.shiny.wasm.repo", prepare_source,
                           fixed = TRUE)))
  }
})

test_that("bundled app rejects a mismatched wasm package version", {
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
  old_options <- options(c("openspecy.shiny.wasm",
                           "openspecy.shiny.wasm.package_version",
                           "openspecy.shiny.wasm.package_sha"))
  old_env <- Sys.getenv("OPENSPECY_SHINY_WASM", unset = NA)
  on.exit(setwd(old_wd), add = TRUE)
  on.exit(options(old_options), add = TRUE)
  on.exit({
    if (is.na(old_env)) Sys.unsetenv("OPENSPECY_SHINY_WASM") else
      Sys.setenv(OPENSPECY_SHINY_WASM = old_env)
  }, add = TRUE)

  options(
    openspecy.shiny.wasm = TRUE,
    openspecy.shiny.wasm.package_version = "0.0.0",
    openspecy.shiny.wasm.package_sha = "test-commit"
  )
  setwd(app_path)

  expect_error(
    sys.source(file.path(app_path, "global.R"), envir = env),
    "pinned build requires 0.0.0"
  )
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
