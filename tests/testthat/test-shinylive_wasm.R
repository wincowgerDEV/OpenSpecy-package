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

shiny_app_source_path <- function(file) {
  installed <- system.file("shiny", file, package = "OpenSpecy")
  if (nzchar(installed)) return(installed)

  workspace <- Sys.getenv("GITHUB_WORKSPACE", unset = "")
  candidates <- c(
    test_path("..", "..", "inst", "shiny", file),
    if (nzchar(workspace)) file.path(workspace, "inst", "shiny", file)
  )
  existing <- candidates[file.exists(candidates)]
  testthat::skip_if(
    length(existing) == 0L,
    "bundled Shiny application sources are unavailable"
  )
  existing[[1L]]
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
  hosted_packages <- .openspecy_app_packages(hosted = TRUE)

  expect_true("local::." %in% roots)
  expect_true(all(hosted_packages %in% roots))
  expect_true(all(c(
    "shiny", "plotly", "data.table", "jsonlite", "OpenSpecy"
  ) %in% c(roots, "OpenSpecy")))
  expect_true(all(c("curl", "scales") %in% hosted_packages))
  expect_true("shinyFiles" %in% .openspecy_app_packages())
  expect_false("shinyFiles" %in% hosted_packages)
  expect_false("shinyFiles" %in% roots)

  app_sources <- unlist(lapply(
    c("global.R", "ui.R", "server.R"),
    function(file) readLines(shiny_app_source_path(file), warn = FALSE)
  ), use.names = FALSE)
  expect_false(any(grepl("shinyFiles::", app_sources, fixed = TRUE)))
})

test_that("Fill Peaks has no compiled baseline runtime dependency", {
  description_path <- test_path("..", "..", "DESCRIPTION")
  source_path <- test_path("..", "..", "R", "subtr_baseline.R")
  if (!file.exists(description_path) || !file.exists(source_path)) {
    skip("Repository source files are not in the package tarball")
  }

  description <- read.dcf(description_path)[1, ]
  hard_fields <- intersect(c("Depends", "Imports", "LinkingTo"),
                           names(description))
  hard_dependencies <- paste(description[hard_fields], collapse = ",") |>
    strsplit(",", fixed = TRUE) |>
    unlist(use.names = FALSE) |>
    trimws() |>
    sub("\\s*\\(.*$", "", x = _)
  roots <- read_wasm_manifest_lines(wasm_manifest_path(
    "app-package-roots.txt"
  ))
  source <- readLines(source_path, warn = FALSE)

  expect_false("baseline" %in% hard_dependencies)
  expect_false("baseline" %in% roots)
  expect_false(any(grepl("baseline::", source, fixed = TRUE)))
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

test_that("pinned wasm artifacts must match their exact package commit", {
  env <- new.env(parent = globalenv())
  source_wasm_tool("check-wasm-artifact.R", env)

  tmp <- file.path(tempdir(), "OpenSpecy-testthat-wasm-artifact")
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  metadata <- file.path(tmp, "metadata")
  image <- file.path(tmp, "image")
  dir.create(metadata, recursive = TRUE, showWarnings = FALSE)
  dir.create(image, recursive = TRUE, showWarnings = FALSE)
  writeBin(charToRaw("library image"), file.path(image, "library.data.gz"))
  writeBin(charToRaw("library metadata"),
           file.path(image, "library.js.metadata"))

  desc <- read.dcf(test_path("..", "..", "DESCRIPTION"))[1, ]
  sha <- strrep("a", 40)
  artifact <- list(
    package = list(name = unname(desc[["Package"]]),
                   version = unname(desc[["Version"]]), commit = sha),
    wasm_build = list(artifact = paste0("openspecy-wasm-", sha))
  )
  resolved <- list(
    package = artifact$package,
    packages = list(list(Package = unname(desc[["Package"]]),
                         Version = unname(desc[["Version"]]))),
    image = lapply(c("library.data.gz", "library.js.metadata"), function(name) {
      path <- file.path(image, name)
      list(name = name, size = unname(file.info(path)$size),
           md5 = unname(tools::md5sum(path)))
    })
  )
  original_images <- resolved$image
  artifact_path <- file.path(metadata, "wasm-app-manifest.json")
  resolved_path <- file.path(metadata, "resolved-wasm-packages.json")
  jsonlite::write_json(artifact, artifact_path, auto_unbox = TRUE)
  jsonlite::write_json(resolved, resolved_path, auto_unbox = TRUE)

  expect_no_error(env$check_wasm_artifact(
    tmp, sha, test_path("..", "..", "DESCRIPTION")
  ))
  artifact$package$commit <- strrep("b", 40)
  jsonlite::write_json(artifact, artifact_path, auto_unbox = TRUE)
  expect_error(env$check_wasm_artifact(
    tmp, sha, test_path("..", "..", "DESCRIPTION")
  ), "artifact package commit mismatch")
  artifact$package$commit <- sha
  jsonlite::write_json(artifact, artifact_path, auto_unbox = TRUE)
  writeBin(charToRaw("library Image"), file.path(image, "library.data.gz"))
  expect_error(env$check_wasm_artifact(
    tmp, sha, test_path("..", "..", "DESCRIPTION")
  ), "MD5 mismatch")

  writeBin(charToRaw("library image"), file.path(image, "library.data.gz"))
  resolved$image <- original_images[1L]
  jsonlite::write_json(resolved, resolved_path, auto_unbox = TRUE)
  expect_error(env$check_wasm_artifact(
    tmp, sha, test_path("..", "..", "DESCRIPTION")
  ), "exactly library.data.gz")

  resolved$image <- original_images
  resolved$image[[1L]]$size <- resolved$image[[1L]]$size + 1
  jsonlite::write_json(resolved, resolved_path, auto_unbox = TRUE)
  expect_error(env$check_wasm_artifact(
    tmp, sha, test_path("..", "..", "DESCRIPTION")
  ), "size mismatch")

  resolved$image <- original_images
  jsonlite::write_json(resolved, resolved_path, auto_unbox = TRUE)
  verified_path <- file.path(tmp, "verified-wasm-packages.json")
  jsonlite::write_json(resolved, verified_path, auto_unbox = TRUE)
  expect_no_error(env$check_wasm_artifact(
    tmp, sha, test_path("..", "..", "DESCRIPTION"), verified_path
  ))
  verified <- resolved
  verified$packages[[1L]]$Version <- "0.0.0"
  jsonlite::write_json(verified, verified_path, auto_unbox = TRUE)
  expect_error(env$check_wasm_artifact(
    tmp, sha, test_path("..", "..", "DESCRIPTION"), verified_path
  ), "does not match")
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

test_that("assembled Pages site separates landing, pkgdown, and app routes", {
  env <- new.env(parent = globalenv())
  source_wasm_tool("check-pages-site.R", env)

  tmp <- file.path(tempdir(), "OpenSpecy-testthat-pages-site")
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  dir.create(file.path(tmp, "app"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(tmp, "pkgdown"), showWarnings = FALSE)
  dir.create(file.path(tmp, "assets"), showWarnings = FALSE)
  root <- c(
    '<title>OpenSpecy | Raman and FTIR</title>',
    '<meta name="description" content="OpenSpecy spectral analysis">',
    '<meta property="og:title" content="OpenSpecy">',
    '<link rel="canonical" href="https://example.test/OpenSpecy/">',
    '<script type="application/ld+json">{}</script>',
    '<main><a href="pkgdown/">Docs</a>',
    '<div data-openspecy-embed>',
    '<iframe id="openspecy-app-frame" src="app/"></iframe>',
    '</div></main>'
  )
  writeLines(root, file.path(tmp, "index.html"))
  writeLines("runExportedApp({});", file.path(tmp, "app", "index.html"))
  writeLines("<!-- Generated by pkgdown -->",
             file.path(tmp, "pkgdown", "index.html"))
  writeLines("body {}", file.path(tmp, "assets", "site.css"))
  writeLines("void 0;", file.path(tmp, "assets", "site.js"))
  writeLines(c("User-agent: *", "Sitemap: https://example.test/sitemap.xml"),
             file.path(tmp, "robots.txt"))
  writeLines(c(
    "<urlset>",
    "<loc>https://example.test/app/</loc>",
    "<loc>https://example.test/pkgdown/</loc>",
    "</urlset>"
  ), file.path(tmp, "sitemap.xml"))

  expect_no_error(env$check_pages_site(tmp, max_bytes = 1024^2))
  writeLines(sub(
    '<iframe id="openspecy-app-frame"',
    '<div class="sourceCode"><iframe id="openspecy-app-frame"',
    root, fixed = TRUE
  ), file.path(tmp, "index.html"))
  expect_error(env$check_pages_site(tmp, max_bytes = 1024^2),
               "rendered as a source-code block")
  writeLines(root, file.path(tmp, "index.html"))

  writeLines('<body data-openspecy-app-placeholder></body>',
             file.path(tmp, "app", "index.html"))
  expect_error(env$check_pages_site(tmp, max_bytes = 1024^2),
               "rejects the shell-only")
  expect_no_error(env$check_pages_site(
    tmp, max_bytes = 1024^2, shell_only = TRUE
  ))
  writeLines("runExportedApp({});", file.path(tmp, "app", "index.html"))
  expect_error(env$check_pages_site(
    tmp, max_bytes = 1024^2, shell_only = TRUE
  ), "requires the explicit")

  dir.create(file.path(tmp, "wasm"))
  expect_error(env$check_pages_site(tmp, max_bytes = 1024^2),
               "must not contain a wasm repository")
})

test_that("Pages shell staging preserves generated sibling routes", {
  env <- new.env(parent = globalenv())
  source_wasm_tool("stage-pages-shell.R", env)

  repo_root <- normalizePath(test_path("..", ".."), winslash = "/")
  tmp <- file.path(tempdir(), "OpenSpecy-testthat-pages-shell")
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  dir.create(file.path(tmp, "app"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(tmp, "pkgdown"), showWarnings = FALSE)
  writeLines("app sentinel", file.path(tmp, "app", "sentinel.txt"))
  writeLines("pkgdown sentinel", file.path(tmp, "pkgdown", "sentinel.txt"))

  expect_no_error(env$stage_pages_shell(tmp, repo_root = repo_root))
  expect_no_error(env$stage_pages_shell(tmp, repo_root = repo_root))
  expect_true(all(file.exists(file.path(tmp, c(
    "index.html", "robots.txt", "sitemap.xml", ".nojekyll",
    "assets/site.css", "assets/site.js", "assets/openspecy-logo.png",
    "assets/favicon.png", "assets/favicon.ico",
    "articles/sop.html", "reference/index.html", "authors.html"
  )))))
  expect_identical(readLines(file.path(tmp, "app", "sentinel.txt")),
                   "app sentinel")
  expect_identical(readLines(file.path(tmp, "pkgdown", "sentinel.txt")),
                   "pkgdown sentinel")
  redirect <- readLines(file.path(tmp, "articles", "sop.html"), warn = FALSE)
  expect_true(any(grepl("../pkgdown/articles/sop.html", redirect,
                         fixed = TRUE)))
  expect_false(any(grepl('url="/pkgdown/', redirect, fixed = TRUE)))
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
  expect_true(any(grepl('dest_dir = "_site/pkgdown"', shinylive,
                         fixed = TRUE)))
  expect_true(any(grepl("tools/wasm/stage-pages-shell.R", shinylive,
                         fixed = TRUE)))
  expect_true(any(grepl("_site/app", shinylive, fixed = TRUE)))
  expect_true(any(grepl("_site/pkgdown", shinylive, fixed = TRUE)))
  expect_false(any(grepl("_site/openspecy", shinylive, fixed = TRUE)))
  expect_true(any(grepl('repository: ${{ github.repository }}', shinylive,
                        fixed = TRUE)))
  expect_false(any(grepl("wincowgerDEV/OpenSpecy-package", shinylive,
                         fixed = TRUE)))
  expect_true(any(grepl(
    "SHINYLIVE_SMOKE_URL=http://127.0.0.1:8080/ ",
    shinylive, fixed = TRUE
  )))
  expect_true(any(grepl('pkgdown_url="${public_url}pkgdown/"', shinylive,
                         fixed = TRUE)))
  expect_false(any(grepl("_site/wasm", c(shinylive, wasm), fixed = TRUE)))
  expect_true(any(grepl("path: _wasm/pinned", wasm, fixed = TRUE)))
  expect_equal(sum(grepl("pak-version: repo", shinylive, fixed = TRUE)), 1L)
  expect_equal(sum(grepl("pak-version: repo", wasm, fixed = TRUE)), 1L)
  expect_equal(sum(grepl("any::pkgdown", shinylive, fixed = TRUE)), 1L)
  expect_true(any(grepl("tools/wasm/build-wasm-repo.ps1", wasm,
                         fixed = TRUE)))
  expect_true(any(grepl("uses: actions/cache@v5", wasm, fixed = TRUE)))
  expect_true(any(grepl("path: _wasm/rwasm-dependency-cache", wasm,
                        fixed = TRUE)))
  expect_true(any(grepl("wasm-deps-v1-", wasm, fixed = TRUE)))
  expect_gte(sum(grepl("wasm-deps-v1-${{ runner.os }}-", wasm,
                       fixed = TRUE)), 2L)
  expect_true(any(grepl("test-dependency-cache.ps1", wasm,
                         fixed = TRUE)))
  expect_true(any(grepl("tools/wasm/test-workspace-path.ps1", wasm,
                         fixed = TRUE)))
  expect_false(any(grepl("r-wasm/actions/build-rwasm@v3", wasm,
                         fixed = TRUE)))
  expect_gte(sum(grepl("check-wasm-artifact.R", shinylive,
                       fixed = TRUE)), 2L)
  expect_true(any(grepl("check-wasm-repo.R", shinylive, fixed = TRUE)))

  driver_path <- test_path("..", "..", "tools", "wasm", "rwasm-build",
                           "Dockerfile")
  expect_true(file.exists(driver_path))
  driver <- readLines(driver_path, warn = FALSE)
  expect_true(any(grepl(
    "ghcr.io/r-wasm/webr@sha256:2bd309d7a4ea1daed82b6fdb8e325b0de715fcd8592c5b6f3b3b88366e70cb76",
    driver, fixed = TRUE
  )))
})

test_that("hosted preflight is exact and the full pre-push gate is unskippable", {
  preflight_path <- test_path("..", "..", "tools", "wasm",
                              "test-shinylive-action.ps1")
  prepush_path <- test_path("..", "..", "tools", "wasm",
                             "test-shinylive-prepush.ps1")
  build_path <- test_path("..", "..", "tools", "wasm",
                           "build-wasm-repo.ps1")
  docker_path <- test_path("..", "..", "tools", "wasm",
                            "docker-preflight.ps1")
  docker_test_path <- test_path("..", "..", "tools", "wasm",
                                 "test-docker-engine.ps1")
  shell_path <- test_path("..", "..", "tools", "wasm",
                           "test-pages-shell.ps1")
  if (!all(file.exists(c(
    preflight_path, prepush_path, build_path, docker_path,
    docker_test_path, shell_path
  )))) {
    skip("Repository-only wasm preflight scripts are not in the package tarball")
  }
  preflight <- readLines(preflight_path, warn = FALSE)
  prepush <- readLines(prepush_path, warn = FALSE)
  build <- readLines(build_path, warn = FALSE)
  docker <- readLines(docker_path, warn = FALSE)
  docker_test <- readLines(docker_test_path, warn = FALSE)
  shell <- readLines(shell_path, warn = FALSE)

  expect_true(any(grepl("git rev-parse HEAD", preflight, fixed = TRUE)))
  expect_true(any(grepl("options(pkgdown.internet=FALSE)", preflight,
                         fixed = TRUE)))
  expect_true(any(grepl("check-wasm-artifact.R", preflight, fixed = TRUE)))
  expect_true(any(grepl("check-wasm-repo.R", preflight, fixed = TRUE)))
  expect_true(any(grepl('$env:R_LIBS_USER = $tools', preflight,
                        fixed = TRUE)))
  expect_true(any(grepl(
    '$env:R_PKG_CACHE_DIR = Join-Path $tools "pkg-cache"',
    preflight, fixed = TRUE
  )))
  expect_true(any(grepl('"--no-lock"', preflight, fixed = TRUE)))
  expect_true(any(grepl("Remove-Item -LiteralPath $tools", preflight,
                        fixed = TRUE)))
  expect_true(any(grepl('Assert-Equal $playwrightVersion "1.61.1"',
                        preflight, fixed = TRUE)))
  expect_true(any(grepl("require('@playwright/test'); require('http-server')",
                        preflight, fixed = TRUE)))
  expect_true(any(grepl("ConnectAsync", preflight, fixed = TRUE)))
  expect_true(any(grepl("RedirectStandardError", preflight, fixed = TRUE)))
  expect_false(any(grepl("defaultLibrary", preflight, fixed = TRUE)))
  expect_true(any(grepl("git status --porcelain", prepush, fixed = TRUE)))
  expect_true(any(grepl("docker-preflight.ps1", prepush, fixed = TRUE)))
  expect_true(any(grepl("Assert-OpenSpecyDockerEngine", prepush,
                         fixed = TRUE)))
  expect_true(any(grepl("build-wasm-repo.ps1", prepush, fixed = TRUE)))
  expect_true(any(grepl("test-shinylive-action.ps1", prepush,
                        fixed = TRUE)))
  expect_true(any(grepl("-StageLibraries", prepush, fixed = TRUE)))
  expect_true(any(grepl("-Bootstrap", prepush, fixed = TRUE)))
  expect_true(any(grepl("FULL SHINYLIVE PRE-PUSH REHEARSAL PASSED", prepush,
                        fixed = TRUE)))
  expect_true(any(grepl("tools/wasm/rwasm-build", build, fixed = TRUE)))
  expect_true(any(grepl("docker-preflight.ps1", build, fixed = TRUE)))
  expect_true(any(grepl("Assert-OpenSpecyDockerEngine", build,
                         fixed = TRUE)))
  expect_true(any(grepl("check-wasm-artifact.R", build, fixed = TRUE)))
  expect_true(any(grepl("DependencyCacheDir", build, fixed = TRUE)))
  expect_true(any(grepl("DependencyCacheSeed", build, fixed = TRUE)))
  expect_true(any(grepl("dependency-cache.ps1", build, fixed = TRUE)))
  expect_true(any(grepl("Get-OpenSpecyCompatibleWasmCacheSeed", build,
                         fixed = TRUE)))
  expect_true(any(grepl("evict-wasm-cache-package.R", build, fixed = TRUE)))
  expect_true(any(grepl("Restoring wasm dependency cache", build,
                        fixed = TRUE)))
  expect_true(any(grepl("git rev-parse HEAD", build, fixed = TRUE)))
  expect_true(any(grepl("git status --porcelain", build, fixed = TRUE)))
  expect_true(any(grepl('"archive", "--format=zip"', build,
                         fixed = TRUE)))
  expect_true(any(grepl(':/github/workspace:ro', build, fixed = TRUE)))
  expect_true(any(grepl(':/github/output', build, fixed = TRUE)))
  expect_true(any(grepl('"/github/output/image"', build, fixed = TRUE)))
  expect_true(any(grepl('Remove-Item -LiteralPath $sourceSnapshot', build,
                         fixed = TRUE)))
  expect_false(any(grepl('$repoRoot + ":/github/workspace"', build,
                          fixed = TRUE)))
  expect_true(any(grepl("workspace-path.ps1", c(preflight, build),
                         fixed = TRUE)))
  path_consumers <- list(preflight, build, shell)
  expect_true(all(vapply(
    path_consumers,
    function(x) any(grepl("Get-OpenSpecyRepoRelativePath", x, fixed = TRUE)),
    logical(1)
  )))
  expect_false(any(grepl("MakeRelativeUri", unlist(path_consumers),
                         fixed = TRUE)))
  expect_true(any(grepl("{{.Server.Version}}", docker, fixed = TRUE)))
  expect_true(any(grepl("{{.OSType}}", docker, fixed = TRUE)))
  expect_true(any(grepl('osType -ne "linux"', docker, fixed = TRUE)))
  expect_true(any(grepl("wsl --update", docker, fixed = TRUE)))
  expect_true(any(grepl("wsl --shutdown", docker, fixed = TRUE)))
  expect_true(any(grepl(
    "No wasm build or cached/native fallback was used", docker, fixed = TRUE
  )))
  expect_true(any(grepl("environment-only check", docker_test,
                         fixed = TRUE)))
  expect_false(any(grepl("git status", docker_test, fixed = TRUE)))
  expect_true(any(grepl("--shell-only", shell, fixed = TRUE)))
  expect_true(any(grepl("action_equivalent = $false", shell, fixed = TRUE)))
  expect_true(any(grepl('wasm_build = "not_run"', shell, fixed = TRUE)))
  expect_true(any(grepl('hosted_webr_browser = "not_run"', shell,
                         fixed = TRUE)))
})

test_that("wasm workspace path resolver handles absolute paths portably", {
  script <- test_path("..", "..", "tools", "wasm",
                      "test-workspace-path.ps1")
  if (!file.exists(script)) {
    skip("Repository-only workspace path contract is not in the package tarball")
  }
  powershell <- Sys.which("powershell.exe")
  if (!nzchar(powershell)) powershell <- Sys.which("pwsh")
  skip_if(!nzchar(powershell), "PowerShell is unavailable")
  output <- system2(
    powershell,
    c("-NoProfile", "-ExecutionPolicy", "Bypass", "-File", shQuote(script)),
    stdout = TRUE, stderr = TRUE
  )
  expect_null(attr(output, "status"), info = paste(output, collapse = "\n"))
  expect_true(any(grepl("emits portable repository-relative paths", output,
                         fixed = TRUE)))
})

test_that("pkgdown installs the checked-out package before rendering", {
  workflow_path <- test_path("..", "..", ".github", "workflows",
                             "pkgdown.yaml")
  if (!file.exists(workflow_path)) {
    skip("Repository-only workflow files are not in the package tarball")
  }

  workflow <- readLines(workflow_path, warn = FALSE)
  install_step <- grep("Install current package source", workflow,
                       fixed = TRUE)
  build_step <- grep("- name: Build site", workflow, fixed = TRUE)

  expect_length(install_step, 1L)
  expect_length(build_step, 1L)
  expect_lt(install_step, build_step)
  expect_true(any(grepl(
    'install.packages(".", repos = NULL, type = "source")',
    workflow, fixed = TRUE
  )))
  expect_true(any(grepl(
    'exists(".fill_peaks_smooth", namespace, inherits = FALSE)',
    workflow, fixed = TRUE
  )))
})

test_that("hosted deployment exports the exact current bundled app", {
  workflow_path <- test_path("..", "..", ".github", "workflows",
                             "deploy-shinylive.yml")
  smoke_path <- test_path("..", "..", "tools", "wasm",
                          "shinylive-smoke.spec.js")
  if (!file.exists(workflow_path) || !file.exists(smoke_path)) {
    skip("Repository-only hosted deployment files are not in the package tarball")
  }

  workflow <- readLines(workflow_path, warn = FALSE)
  smoke <- readLines(smoke_path, warn = FALSE)
  install_step <- grep("Install current package and app source", workflow,
                       fixed = TRUE)
  build_step <- grep("- name: Build pkgdown site", workflow, fixed = TRUE)

  expect_length(install_step, 1L)
  expect_length(build_step, 1L)
  expect_lt(install_step, build_step)
  expect_true(any(grepl(
    'install.packages(".", repos = NULL, type = "source")',
    workflow, fixed = TRUE
  )))
  expect_true(any(grepl("tools::md5sum", workflow, fixed = TRUE)))
  expect_true(any(grepl("options(pkgdown.internet = FALSE)", workflow,
                         fixed = TRUE)))
  expect_true(any(grepl('dest_dir = "_site/pkgdown"', workflow,
                         fixed = TRUE)))
  expect_true(any(grepl("stage-pages-shell.R", workflow, fixed = TRUE)))
  expect_true(any(grepl("check-pages-site.R _site", workflow,
                         fixed = TRUE)))
  expect_true(any(grepl('--app-dir "inst/shiny"', workflow, fixed = TRUE)))
  expect_true(any(grepl("for attempt in 1 2", workflow, fixed = TRUE)))
  expect_true(any(grepl("Upload hosted smoke diagnostics", workflow,
                        fixed = TRUE)))
  expect_true(any(grepl("pinned-wasm-library.json", workflow, fixed = TRUE)))
  expect_true(any(grepl(
    'grep -q "${PACKAGE_SHA}" <<< "$pin_body"', workflow, fixed = TRUE
  )))
  expect_true(any(grepl(
    "test.setTimeout(largeUploads.length ? 2400000 : 1800000)", smoke,
    fixed = TRUE
  )))
  expect_true(any(grepl("timeout: 600000", smoke, fixed = TRUE)))
  expect_true(any(grepl("toBeChecked()", smoke, fixed = TRUE)))
  expect_true(any(grepl('toHaveValue("Top Matches")', smoke,
                        fixed = TRUE)))
  expect_false(any(grepl("downloadSelectize", smoke, fixed = TRUE)))
  expect_true(any(grepl("verifyNativeDownload", smoke, fixed = TRUE)))
  expect_true(any(grepl("savedFirstBytesHex", smoke, fixed = TRUE)))
  expect_true(any(grepl("contentType", smoke, fixed = TRUE)))
  expect_true(any(grepl("disposition", smoke, fixed = TRUE)))
  expect_true(any(grepl("requestUrl", smoke, fixed = TRUE)))
  expect_true(any(grepl("clickResponse", smoke, fixed = TRUE)))
  expect_false(any(grepl('if (!clickResponse.length)', smoke,
                         fixed = TRUE)))
  expect_true(any(grepl("probeDownloadEndpoint", smoke, fixed = TRUE)))
  expect_true(any(grepl("native browser download", smoke, fixed = TRUE)))
  expect_true(any(grepl("shinylive-download-", smoke,
                        fixed = TRUE)))
  expect_true(any(grepl('fetch(element.href, { cache: "no-store" })', smoke,
                        fixed = TRUE)))
  expect_false(any(grepl('downloadFailure === "canceled"', smoke,
                         fixed = TRUE)))
  expect_false(any(grepl("handler checks remain authoritative", smoke,
                         ignore.case = TRUE)))
  expect_true(all(vapply(
    c(
      "Test Data", "Test Map", "User Metadata", "Processed Spectra",
      "Top Matches", "Thresholded Particles"
    ),
    function(label) any(grepl(paste0('label: "', label, '"'), smoke,
                              fixed = TRUE)),
    logical(1)
  )))
  expect_true(any(grepl('download.path()', smoke, fixed = TRUE)))
  expect_true(any(grepl('fs.readFileSync(downloadPath)', smoke,
                        fixed = TRUE)))
  expect_true(any(grepl('locator("#heatmap_frame")', smoke,
                        fixed = TRUE)))
  expect_true(any(grepl("tinyEnviFiles", smoke, fixed = TRUE)))
  expect_true(any(grepl("#openspecy_workerfs_files", smoke, fixed = TRUE)))
  expect_true(any(grepl("mountedInput.setInputFiles(mapUploadPath)", smoke,
                        fixed = TRUE)))
  expect_true(any(grepl("OPENSPECY_SMOKE_LARGE_UPLOAD", smoke,
                        fixed = TRUE)))
  expect_true(any(grepl("OPENSPECY_SMOKE_LARGE_UPLOADS", smoke,
                        fixed = TRUE)))
  expect_true(any(grepl('candidate.type === "heatmap"', smoke,
                        fixed = TRUE)))
  expect_true(any(grepl('label: "Test Map Top Matches"', smoke,
                        fixed = TRUE)))
  expect_true(any(grepl('"CA small UF.dat"', smoke, fixed = TRUE)))
  expect_true(any(grepl("probeEndpoint: false", smoke, fixed = TRUE)))
  top_n_locator <- grep(
    'const mapTopNInput = appFrame.locator("#top_n_input")',
    smoke, fixed = TRUE
  )
  top_n_tab <- grep('name: "Identification", exact: true', smoke,
                    fixed = TRUE)
  top_n_fill <- grep('await mapTopNInput.fill("1")', smoke, fixed = TRUE)
  top_n_blur <- grep('await mapTopNInput.press("Tab")', smoke, fixed = TRUE)
  top_n_lines <- grep("toHaveLength(209)", smoke, fixed = TRUE)
  expect_length(top_n_locator, 1L)
  expect_length(top_n_tab, 1L)
  expect_length(top_n_fill, 1L)
  expect_length(top_n_blur, 1L)
  expect_length(top_n_lines, 1L)
  expect_lt(top_n_tab, top_n_locator)
  expect_lt(top_n_locator, top_n_fill)
  expect_lt(top_n_fill, top_n_blur)
  expect_lt(top_n_blur, top_n_lines)
  expect_true(any(grepl("toHaveLength(209)", smoke, fixed = TRUE)))
  expect_true(any(grepl("cannot allocate vector", smoke, fixed = TRUE)))
  expect_true(any(grepl("stableFor: 1500", smoke, fixed = TRUE)))
  expect_true(any(grepl(
    'filenamePattern: /^Thresholded-Particles-.*\\.zip$/i',
    smoke, fixed = TRUE
  )))
  expect_true(any(grepl('contentTypePattern: /^application\\/zip/i',
                        smoke, fixed = TRUE)))
  expect_true(any(grepl('expectedPrefix: Buffer.from("PK", "ascii")',
                        smoke, fixed = TRUE)))
  particle_outputs <- grep(
    'const selectedParticleOutputs = new Set(["details", "summary"])',
    smoke, fixed = TRUE
  )
  particle_output_names <- grep(
    'const particleOutputNames = ["details", "processed", "summary", "figures"]',
    smoke, fixed = TRUE
  )
  thresholded_label <- grep('label: "Thresholded Particles"', smoke,
                            fixed = TRUE)
  expect_length(particle_outputs, 1L)
  expect_length(particle_output_names, 1L)
  expect_length(thresholded_label, 1L)
  expect_lt(particle_outputs, thresholded_label)
  expect_lt(particle_output_names, thresholded_label)
  thresholded_call <- smoke[grep('label: "Thresholded Particles"', smoke,
                                 fixed = TRUE)[1L] + 0:12]
  expect_true(any(grepl("probeEndpoint: false", thresholded_call,
                        fixed = TRUE)))
  expect_true(any(grepl("eventTimeout: 300000", thresholded_call,
                        fixed = TRUE)))
  expect_true(any(grepl('toContain("particle_summary.csv")', smoke,
                        fixed = TRUE)))
  expect_true(any(grepl('require("./zip-entries")', smoke, fixed = TRUE)))
  expect_false(any(grepl('require("child_process")', smoke,
                         fixed = TRUE)))
  expect_false(any(grepl('"tar"', smoke, fixed = TRUE)))

  zip_entries_path <- test_path("..", "..", "tools", "wasm",
                                "zip-entries.js")
  zip_entries <- readLines(zip_entries_path, warn = FALSE)
  expect_true(any(grepl("function parseZipEntryNames", zip_entries,
                        fixed = TRUE)))
  expect_true(any(grepl("ZIP central-directory self-test passed", zip_entries,
                        fixed = TRUE)))
  expect_true(any(grepl('new URL("pkgdown/", url)', smoke,
                         fixed = TRUE)))
  expect_true(any(grepl('a[href^="pkgdown/"]', smoke, fixed = TRUE)))

  local_smoke_path <- test_path("..", "..", "tools",
                                "shiny-local-smoke.spec.js")
  local_smoke <- readLines(local_smoke_path, warn = FALSE)
  expect_true(any(grepl("CA_tiny_map.zip", local_smoke, fixed = TRUE)))
  expect_true(any(grepl("Thresholded Particles", local_smoke,
                        fixed = TRUE)))
  expect_true(any(grepl("fetchDownload", local_smoke, fixed = TRUE)))
  expect_true(any(grepl("topMatches.elapsed", local_smoke,
                        fixed = TRUE)))
  expect_true(any(grepl('"CA small UF.dat"', local_smoke, fixed = TRUE)))
  expect_true(any(grepl("toHaveLength(2081)", local_smoke,
                        fixed = TRUE)))

  quality_gate_path <- test_path(
    "..", "..", ".agents", "skills", "openspecy-run-quality-gates",
    "scripts", "quality-gates.ps1"
  )
  quality_gate <- readLines(quality_gate_path, warn = FALSE)
  expect_true(any(grepl("[switch]$HostedAppStatic", quality_gate,
                        fixed = TRUE)))
  expect_true(any(grepl("filter = 'shinylive_wasm'", quality_gate,
                        fixed = TRUE)))
  expect_true(any(grepl('Get-ChildItem "tools/wasm" -File -Filter *.js',
                        quality_gate, fixed = TRUE)))
  expect_true(any(grepl('node.Source "tools/wasm/zip-entries.js"', quality_gate,
                        fixed = TRUE)))
  expect_true(any(grepl("Language.Parser]::ParseFile", quality_gate,
                        fixed = TRUE)))
})

test_that("static landing and Shiny app provide the embed handshake", {
  app_path <- run_app(test_mode = TRUE)
  ui_source <- readLines(file.path(app_path, "ui.R"), warn = FALSE)
  bridge_path <- file.path(app_path, "www", "parent-frame.js")

  expect_true(file.exists(bridge_path))
  expect_true(any(grepl("parent-frame.js", ui_source, fixed = TRUE)))
  bridge <- readLines(bridge_path, warn = FALSE)
  expect_true(any(grepl("shiny:idle.openspecyParent", bridge,
                        fixed = TRUE)))
  expect_true(any(grepl("shiny:connected.openspecyParent", bridge,
                        fixed = TRUE)))
  expect_true(any(grepl("notifyIfConnectedAndIdle", bridge,
                        fixed = TRUE)))
  expect_true(any(grepl("readyProbeTimer", bridge, fixed = TRUE)))
  expect_true(any(grepl("probeReadyState", bridge, fixed = TRUE)))
  expect_equal(sum(grepl("probeReadyState();", bridge, fixed = TRUE)), 2L)
  expect_true(any(grepl("data-openspecy-parent-bridge", bridge,
                        fixed = TRUE)))
  expect_true(any(grepl("window.jQuery || window.$", bridge, fixed = TRUE)))
  expect_true(any(grepl("data-openspecy-parent-bindings", bridge,
                        fixed = TRUE)))
  expect_true(any(grepl("window.top.postMessage", bridge, fixed = TRUE)))
  expect_true(any(grepl("openspecy:ready", bridge, fixed = TRUE)))
  expect_true(any(grepl("shiny:busy.openspecyBusy", bridge, fixed = TRUE)))
  expect_true(any(grepl("openspecy-analysis-phase", bridge, fixed = TRUE)))
  expect_false(any(grepl("openspecy-analysis-complete", bridge,
                         fixed = TRUE)))
  expect_true(any(grepl("openspecy-upload-materialized", bridge,
                        fixed = TRUE)))
  expect_true(any(grepl("data-openspecy-materialized-files", bridge,
                        fixed = TRUE)))
  expect_true(any(grepl("Array.isArray(state.files)", bridge,
                        fixed = TRUE)))
  expect_false(any(grepl("shiny:value.openspecyBusy", bridge, fixed = TRUE)))
  expect_true(any(grepl("openspecy-busy-visible", bridge, fixed = TRUE)))
  expect_true(any(grepl("openspecy:workerfs", bridge, fixed = TRUE)))
  expect_true(any(grepl('Shiny.setInputValue(', bridge, fixed = TRUE)))
  expect_true(any(grepl('action: "upload"', bridge, fixed = TRUE)))
  expect_true(any(grepl("data-openspecy-upload-status", bridge,
                        fixed = TRUE)))
  expect_false(any(grepl("nativeWasmLimit", bridge, fixed = TRUE)))

  readme_path <- test_path("..", "..", "README.md")
  site_dir <- test_path("..", "..", "site")
  homepage_path <- file.path(site_dir, "index.html")
  script_path <- file.path(site_dir, "assets", "site.js")
  css_path <- file.path(site_dir, "assets", "site.css")
  robots_path <- file.path(site_dir, "robots.txt")
  sitemap_path <- file.path(site_dir, "sitemap.xml")
  pkgdown_config <- test_path("..", "..", "_pkgdown.yml")
  required <- c(
    readme_path, homepage_path, script_path, css_path, robots_path,
    sitemap_path, pkgdown_config
  )
  if (!all(file.exists(required))) {
    skip("Repository-only hosted-site sources are not in the package tarball")
  }

  readme <- readLines(readme_path, warn = FALSE)
  homepage <- readLines(homepage_path, warn = FALSE)
  homepage_text <- paste(homepage, collapse = "\n")
  script <- readLines(script_path, warn = FALSE)
  css <- readLines(css_path, warn = FALSE)
  robots <- readLines(robots_path, warn = FALSE)
  sitemap <- readLines(sitemap_path, warn = FALSE)
  pkgdown <- readLines(pkgdown_config, warn = FALSE)
  expect_false(any(grepl("data-openspecy-embed", readme, fixed = TRUE)))
  expect_false(any(grepl("openspecy-app-frame", readme, fixed = TRUE)))
  expect_false(file.exists(test_path("..", "..", "pkgdown", "index.md")))
  expect_false(file.exists(test_path("..", "..", "pkgdown", "extra.js")))
  expect_false(file.exists(test_path("..", "..", "pkgdown", "extra.css")))
  expect_true(any(grepl("data-openspecy-embed", homepage, fixed = TRUE)))
  expect_true(any(grepl('src="app/"', homepage, fixed = TRUE)))
  expect_true(any(grepl('href="pkgdown/"', homepage, fixed = TRUE)))
  expect_false(any(grepl('src="/app/"|href="/pkgdown/"', homepage)))
  expect_equal(sum(grepl("<h1[ >]", homepage)), 1L)
  expect_true(any(grepl(
    "OpenSpecy | Free Raman &amp; FTIR Spectral Analysis App",
    homepage, fixed = TRUE
  )))
  expect_true(any(grepl('<meta name="description"', homepage,
                         fixed = TRUE)))
  expect_true(any(grepl('<link rel="canonical"', homepage,
                         fixed = TRUE)))
  expect_true(any(grepl('property="og:title"', homepage, fixed = TRUE)))
  expect_true(any(grepl('name="twitter:card"', homepage, fixed = TRUE)))
  expect_true(any(grepl('type="application/ld+json"', homepage,
                         fixed = TRUE)))
  expect_true(all(vapply(
    c('id="start"', 'id="web-app"', 'id="learn"', 'id="science"',
      'id="partners"', 'id="contact"'),
    function(section) any(grepl(section, homepage, fixed = TRUE)),
    logical(1)
  )))
  expect_true(any(grepl("y2F4Fu6A4aA&amp;list=PLqdH8O1nalYa4a8JXQ6GbNsH3YQV_aY7g",
                        homepage, fixed = TRUE)))
  pew_acknowledgement <- paste0(
    "Support for this project was provided by the Pew-Gerstner Fellows ",
    "Program in Marine Conservation at The Pew Charitable Trusts. The views ",
    "expressed herein are those of the author(s) and do not necessarily ",
    "reflect the views of The Pew Charitable Trusts."
  )
  expect_true(grepl(pew_acknowledgement, homepage_text, fixed = TRUE))
  expect_true(grepl(
    paste0(
      "https://www.youtube-nocookie.com/embed/8zrlQeTCwkQ?autoplay=1",
      "&amp;mute=1&amp;playsinline=1&amp;rel=0"
    ), homepage_text, fixed = TRUE
  ))
  expect_true(any(grepl('class="hero-video-card"', homepage,
                         fixed = TRUE)))
  expect_false(any(grepl('class="spectrum-card"', homepage,
                          fixed = TRUE)))
  expect_true(any(grepl("Walking Softer", homepage, fixed = TRUE)))
  expect_true(all(vapply(
    c("10.1021/acs.analchem.5c00962", "10.1021/acs.analchem.1c00123",
      "mailto:wincowger@gmail.com", "youtube-nocookie.com"),
    function(marker) grepl(marker, homepage_text, fixed = TRUE),
    logical(1)
  )))
  json_text <- sub(
    '(?s).*<script type="application/ld\\+json">[[:space:]]*(.*?)[[:space:]]*</script>.*',
    "\\1", homepage_text, perl = TRUE
  )
  structured <- jsonlite::fromJSON(json_text, simplifyVector = FALSE)
  expect_true(structured$`@type` %in% c(
    "WebApplication", "SoftwareApplication"
  ))
  expect_false(any(grepl("requestFullscreen", script, fixed = TRUE)))
  expect_true(any(grepl("openspecy-app-fullscreen-open", script,
                         fixed = TRUE)))
  expect_true(any(grepl("DOMContentLoaded", script, fixed = TRUE)))
  expect_true(any(grepl("initVideo", script, fixed = TRUE)))
  expect_true(any(grepl("container.replaceChildren(frame)", script,
                         fixed = TRUE)))
  expect_true(any(grepl("event.origin !== window.location.origin", script,
                         fixed = TRUE)))
  expect_false(any(grepl(
    'event.key !== "Escape" || !shell.classList.contains("is-fullscreen")',
    script, fixed = TRUE
  )))
  expect_true(any(grepl("app-shell.is-fullscreen", css,
                         fixed = TRUE)))
  expect_true(any(grepl("Sitemap: https://wincowgerdev.github.io/",
                         robots, fixed = TRUE)))
  expect_true(any(grepl("OpenSpecy-package/pkgdown/", sitemap,
                         fixed = TRUE)))
  expect_true(any(grepl(
    "url: https://wincowgerdev.github.io/OpenSpecy-package/pkgdown/",
    pkgdown, fixed = TRUE
  )))
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
    expect_true(any(grepl("patch_shinylive_workerfs", prepare_source,
                          fixed = TRUE)))
    workerfs_patch <- readLines(test_path(
      "..", "..", "tools", "wasm", "patch-shinylive-workerfs.R"
    ), warn = FALSE)
    workerfs_bridge <- readLines(test_path(
      "..", "..", "tools", "wasm", "shinylive-workerfs-bridge.js"
    ), warn = FALSE)
    expect_true(any(grepl("expected_sha256", workerfs_patch,
                          fixed = TRUE)))
    expect_true(any(grepl("rawToChar(readBin", workerfs_patch,
                          fixed = TRUE)))
    expect_true(any(grepl("OPENSPECY_WORKERFS_BRIDGE_V1", workerfs_bridge,
                          fixed = TRUE)))
    expect_true(any(grepl('fs.mount("WORKERFS"', workerfs_bridge,
                          fixed = TRUE)))
    expect_true(any(grepl("handle.webRProxy.webR.FS", workerfs_bridge,
                          fixed = TRUE)))
    expect_true(any(grepl("openspecyWorkerfsCleanup(fs)", workerfs_bridge,
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
