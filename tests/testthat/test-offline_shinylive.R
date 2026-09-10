offline_repo_path <- function(...) {
  path <- test_path("..", "..", ...)
  if(!file.exists(path) && !dir.exists(path)) {
    skip("Repository-only offline distribution sources are unavailable")
  }
  path
}

read_offline_source <- function(...) {
  readLines(offline_repo_path(...), warn = FALSE)
}

test_that("offline workflow consumes one exact successful Pages artifact", {
  workflow <- read_offline_source(
    ".github", "workflows", "build-offline-shinylive.yml"
  )

  expect_true(any(grepl(
    'workflows: ["Build and deploy Shinylive app"]', workflow, fixed = TRUE
  )))
  expect_true(any(grepl(
    "github.event.workflow_run.conclusion == 'success'", workflow, fixed = TRUE
  )))
  expect_true(any(grepl(
    '.name == $workflow and .conclusion == "success"', workflow, fixed = TRUE
  )))
  expect_true(any(grepl(
    'test("^openspecy-pages-[0-9a-f]{40}$")', workflow, fixed = TRUE
  )))
  expect_true(any(grepl(
    '"${source_event}" != "workflow_dispatch"', workflow, fixed = TRUE
  )))
  expect_true(any(grepl(
    '"${source_head_sha}" != "${package_sha}"', workflow, fixed = TRUE
  )))
  expect_true(any(grepl(
    'ref: ${{ steps.source.outputs.package_sha }}', workflow, fixed = TRUE
  )))
  expect_true(any(grepl(
    'name: ${{ steps.source.outputs.artifact_name }}', workflow, fixed = TRUE
  )))
  expect_true(any(grepl('repository: ${{ github.repository }}', workflow,
                        fixed = TRUE)))
  expect_true(any(grepl('run-id: ${{ env.PAGES_RUN_ID }}', workflow,
                        fixed = TRUE)))
  expect_true(any(grepl("actions/download-artifact@v7", workflow,
                        fixed = TRUE)))
  expect_true(any(grepl("_wasm/wasm-app-manifest.json", workflow,
                        fixed = TRUE)) || any(grepl(
    "package-offline", workflow, fixed = TRUE
  )))
})

test_that("offline workflow builds stable archives for all five targets", {
  workflow <- read_offline_source(
    ".github", "workflows", "build-offline-shinylive.yml"
  )
  targets <- c(
    "windows-amd64", "macos-amd64", "macos-arm64",
    "linux-amd64", "linux-arm64"
  )
  for(target in targets) {
    expect_true(any(grepl(
      paste0("openspecy-offline-", target, "-${{ env.PACKAGE_SHA }}"),
      workflow, fixed = TRUE
    )), info = target)
  }
  expect_true(any(grepl("actions/setup-go@v6", workflow, fixed = TRUE)))
  expect_true(any(grepl("CGO_ENABLED=0", workflow, fixed = TRUE)))
  expect_true(any(grepl("go -C tools/offline test ./...", workflow,
                        fixed = TRUE)))
  expect_true(any(grepl('sha256sum --check "${archive}.sha256"', workflow,
                        fixed = TRUE)))
  expect_equal(sum(grepl("compression-level: 0", workflow, fixed = TRUE)), 5L)
  expect_equal(sum(grepl("Upload .* offline bundle", workflow)), 5L)
})

test_that("native launcher is relative, loopback-only, and app-first", {
  launcher <- read_offline_source(
    "tools", "offline", "internal", "launcher", "launcher.go"
  )
  source <- paste(launcher, collapse = "\n")

  expect_match(source, 'os\\.Executable\\(\\)')
  expect_match(source, 'filepath\\.Join\\(filepath\\.Dir\\(absolute\\), "site"\\)')
  expect_match(source, 'loopbackAddress = "127\\.0\\.0\\.1:0"')
  expect_match(source, 'http://127\\.0\\.0\\.1:%d/app/')
  expect_false(grepl("0.0.0.0", source, fixed = TRUE))
  expect_true(all(c("rundll32.exe", "open", "xdg-open") %in%
                    unlist(regmatches(source, gregexpr(
                      "rundll32\\.exe|xdg-open|open", source
                    )))))

  module <- read_offline_source("tools", "offline", "go.mod")
  expect_false(any(grepl("^require\\b", trimws(module))))
  expect_false(any(grepl(
    "Rscript|python|node", launcher, ignore.case = TRUE
  )))
})

test_that("offline packager records exact identity and complete checksums", {
  packager <- read_offline_source(
    "tools", "offline", "internal", "bundle", "bundle.go"
  )
  source <- paste(packager, collapse = "\n")

  expect_match(source, '"OpenSpecy-offline-" \\+ config\\.PackageSHA')
  expect_match(source, '"site"')
  expect_match(source, '"README-OFFLINE\\.txt"')
  expect_match(source, '"offline-manifest\\.json"')
  expect_match(source, '"SHA256SUMS\\.txt"')
  expect_match(source, '"_wasm", "wasm-app-manifest\\.json"')
  expect_match(source, '"pinned-wasm-library\\.json"')
  expect_match(source, '"resolved-wasm-packages\\.json"')
  expect_match(source, 'identity\\.Package\\.Commit != packageSHA')
  expect_match(source, '"openspecy-wasm-" \\+ packageSHA')
  expect_match(source, "hashZipFile\\(entry\\)")
  expect_match(source, "validateLauncherBinary\\(config\\.Launcher")
  expect_match(source, "validatePortableRelativePath")
  expect_match(source, "duplicate or colliding archive entries")
  expect_match(source, "verifyArchiveCompanion\\(config\\.Output\\)")
  expect_match(source, 'Site\\.EntryPoint = "site/app/"')
  expect_match(source, "You do not need\\s+R, Python, Node")
})

test_that("offline browser acceptance blocks all non-loopback requests", {
  workflow <- read_offline_source(
    ".github", "workflows", "build-offline-shinylive.yml"
  )
  smoke <- read_offline_source(
    "tools", "offline", "offline-smoke.spec.js"
  )
  source <- paste(smoke, collapse = "\n")

  expect_true(any(grepl("OpenSpecy Offline acceptance", workflow,
                        fixed = TRUE)))
  expect_true(any(grepl("--no-browser --write-url", workflow,
                        fixed = TRUE)))
  expect_true(any(grepl("offline-smoke.spec.js", workflow, fixed = TRUE)))
  expect_match(source, 'entry\\.hostname\\)\\.toBe\\("127\\.0\\.0\\.1"\\)')
  expect_match(source, 'entry\\.pathname\\)\\.toBe\\("/app/"\\)')
  expect_match(source, 'page\\.goto\\(offlineUrl')
  expect_lt(
    regexpr('page.goto\\(offlineUrl', source),
    regexpr('page.goto\\(new URL\\("/", entry\\)\\.href', source)
  )
  expect_match(source, 'serviceWorkers: "allow"')
  expect_match(source, 'proxy: \\{ server: "http://127\\.0\\.0\\.1:9"')
  expect_match(source, 'route\\.abort\\("blockedbyclient"\\)')
  expect_match(source, 'page\\.on\\("websocket"')
  expect_match(source, 'expect\\(\\[\\.\\.\\.externalAttempts\\]\\)\\.toEqual\\(\\[\\]\\)')
  expect_match(source, 'expect\\(\\[\\.\\.\\.externalWebSockets\\]\\)\\.toEqual\\(\\[\\]\\)')
  expect_match(source, 'page\\.goto\\(new URL\\("/", entry\\)\\.href')
  expect_match(source, 'locator\\("\\[data-video-embed\\]"\\)\\)\\.toHaveCount\\(2\\)')
  expect_match(source, 'locator\\("\\[data-video-embed\\] iframe"\\)\\)\\.toHaveCount\\(0\\)')
  expect_match(source, 'selectDownload\\(app, "Test Data"\\)')
  expect_match(source, "raman_hdpe-offline\\.csv")
  expect_match(source, 'lib_type: "medoid"')
  expect_match(source, 'selectDownload\\(app, "Top Matches"\\)')
  expect_match(source, 'quant_ratio_name: "Offline carbonyl"')
  expect_match(source, 'selectDownload\\(app, "Processed Spectra"\\)')
})
