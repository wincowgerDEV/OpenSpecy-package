test_that("AWS catalog covers every public library type", {
  catalog <- .openspecy_library_catalog()
  expected <- c(
    "derivative", "nobaseline", "raw", "medoid_derivative",
    "medoid_nobaseline", "model_derivative", "model_nobaseline"
  )

  expect_identical(catalog$type, expected)
  expect_identical(catalog$filename, paste0(expected, ".rds"))
  expect_identical(names(catalog), c("type", "filename"))
  versions <- .openspecy_library_versions()
  expect_identical(unique(versions$type), expected)
  expect_identical(nrow(versions), 14L)
  expect_identical(
    sort(unique(versions$date)),
    as.Date(c("2026-09-22", "2026-10-21"))
  )
  expect_false("aws" %in% names(formals(get_lib)))
  expect_true("date" %in% names(formals(get_lib)))
})

test_that("get_lib() builds AWS URLs including raw and S3 version IDs", {
  tmp <- tempfile("OpenSpecy-get-lib-")
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  urls <- character()
  metadata_urls <- character()

  local_mocked_bindings(
    .library_remote_metadata = function(url) {
      metadata_urls <<- c(metadata_urls, url)
      list(date = as.Date("2026-09-22"), version_id = "test-version")
    },
    download.file = function(url, destfile, mode, ...) {
      urls <<- c(urls, url)
      saveRDS(list(downloaded = TRUE), destfile)
      invisible(0L)
    },
    .package = "OpenSpecy"
  )

  messages <- capture.output(
    get_lib(c("raw", "derivative"), path = tmp, revision = "test-version"),
    type = "message"
  )
  expect_match(paste(messages, collapse = "\n"), "from AWS")
  expect_match(paste(messages, collapse = "\n"),
               "dated 2026-09-22.*AWS version test-version")
  expect_identical(
    urls,
    c(
      "https://d2jrxerjcsjhs7.cloudfront.net/raw.rds?versionId=test-version",
      "https://d2jrxerjcsjhs7.cloudfront.net/derivative.rds?versionId=test-version"
    )
  )
  expect_identical(metadata_urls, urls)
  expect_true(all(file.exists(file.path(tmp, c("raw.rds", "derivative.rds")))))
})

test_that("get_lib() defaults to the current unversioned AWS object", {
  tmp <- tempfile("OpenSpecy-get-latest-")
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  download_url <- NULL
  local_mocked_bindings(
    .library_remote_metadata = function(url) {
      list(
        date = as.Date("2026-10-21"),
        version_id = "current-medoid-version"
      )
    },
    download.file = function(url, destfile, mode, ...) {
      download_url <<- url
      saveRDS(list(downloaded = TRUE), destfile)
      invisible(0L)
    },
    .package = "OpenSpecy"
  )

  messages <- capture.output(
    result <- get_lib("medoid_derivative", path = tmp), type = "message"
  )

  expect_identical(
    download_url,
    "https://d2jrxerjcsjhs7.cloudfront.net/medoid_derivative.rds"
  )
  expect_match(paste(messages, collapse = "\n"),
               "dated 2026-10-21.*current-medoid-version")
  expect_null(result)
})

test_that("get_lib() resolves each type to the closest catalogued date", {
  tmp <- tempfile("OpenSpecy-get-dated-")
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  urls <- character()
  current <- c(
    raw = "LMOcxnkXhqXG_vVdXLY33smJXI9jtMK1",
    derivative = "PYhevbuo1qsTfvGyV8952X6oOk863L5k"
  )

  local_mocked_bindings(
    .library_remote_metadata = function(url) {
      type <- sub("\\.rds$", "", basename(url))
      list(date = as.Date("2026-10-21"), version_id = unname(current[[type]]))
    },
    download.file = function(url, destfile, mode, ...) {
      urls <<- c(urls, url)
      saveRDS(list(downloaded = TRUE), destfile)
      invisible(0L)
    },
    .package = "OpenSpecy"
  )

  messages <- capture.output(
    get_lib(c("raw", "derivative"), path = tmp, date = "2026-10-01"),
    type = "message"
  )
  expect_identical(
    urls,
    c(
      paste0(
        "https://d2jrxerjcsjhs7.cloudfront.net/raw.rds?versionId=",
        "ysLLpg5ORhA2sWl6R0hnhaV7CfROPOEU"
      ),
      paste0(
        "https://d2jrxerjcsjhs7.cloudfront.net/derivative.rds?versionId=",
        "plEFh7vekJt_2rcBqEud6KeNTU07FfUy"
      )
    )
  )
  expect_match(paste(messages, collapse = "\n"),
               "dated 2026-09-22 for requested date 2026-10-01")
  expect_equal(sum(grepl("Downloaded .* dated 2026-09-22", messages)), 2L)
})

test_that("date parsing and nearest-date ties are deterministic", {
  expect_identical(.library_target_date(as.Date("2026-10-01")),
                   as.Date("2026-10-01"))
  expect_identical(.library_target_date("2026-10-01"),
                   as.Date("2026-10-01"))
  expect_error(.library_target_date("2026-02-30"), "valid calendar date")
  expect_error(.library_target_date("10/01/2026"), "YYYY-MM-DD")
  expect_error(.library_target_date(c("2026-10-01", "2026-10-02")),
               "one Date")

  local_mocked_bindings(
    .openspecy_library_versions = function() {
      data.frame(
        type = c("raw", "raw"),
        date = as.Date(c("2026-01-01", "2026-01-03")),
        version_id = c("earlier", "later"),
        stringsAsFactors = FALSE
      )
    },
    .package = "OpenSpecy"
  )
  selected <- .library_version_nearest(
    "raw", as.Date("2026-01-02"),
    list(date = as.Date("2026-02-01"), version_id = "current")
  )
  expect_identical(selected$version_id, "earlier")

  current <- list(date = as.Date("2026-02-01"), version_id = "future-current")
  selected <- .library_version_nearest(
    "raw", as.Date("2026-01-30"), current
  )
  expect_identical(selected$version_id, "future-current")
})

test_that("library response headers provide strict release metadata", {
  headers <- c(
    "HTTP/1.1 200 OK\r\n",
    "Last-Modified: Wed, 21 Oct 2026 03:09:47 GMT\r\n",
    "x-amz-version-id: exact-version\r\n"
  )
  expect_identical(.library_header_value(headers, "last-modified"),
                   "Wed, 21 Oct 2026 03:09:47 GMT")
  expect_identical(
    .library_http_date("Wed, 21 Oct 2026 03:09:47 GMT"),
    as.Date("2026-10-21")
  )
  expect_null(.library_header_value(headers, "missing"))
  expect_error(.library_http_date("not an HTTP date"), "invalid Last-Modified")
})

test_that("date and exact revision policies cannot be combined", {
  expect_error(
    get_lib("raw", path = tempfile(), revision = "exact",
            date = "2026-10-01"),
    "only one"
  )

  tmp <- tempfile("OpenSpecy-wrong-revision-")
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  local_mocked_bindings(
    .library_remote_metadata = function(url) {
      list(date = as.Date("2026-10-21"), version_id = "different")
    },
    download.file = function(...) stop("download should not start"),
    .package = "OpenSpecy"
  )
  expect_error(
    get_lib("raw", path = tmp, revision = "exact"),
    "instead of requested revision"
  )
})

test_that("named revisions pin each requested library independently", {
  revisions <- c(raw = "raw-version", derivative = "derivative-version")
  expect_identical(.library_revision(revisions, "raw"), "raw-version")
  expect_identical(
    .library_revision(revisions, "derivative"), "derivative-version"
  )
  expect_error(.library_revision(unname(revisions), "raw"), "must be named")
})

test_that("all latest AWS libraries download, load, and match", {
  skip_on_cran()
  skip_if_not(
    identical(Sys.getenv("OPENSPECY_RUN_AWS_LIBRARY_TESTS"), "true"),
    "Set OPENSPECY_RUN_AWS_LIBRARY_TESTS=true for the large AWS integration"
  )
  skip_if_offline(host = "d2jrxerjcsjhs7.cloudfront.net")

  tmp <- tempfile("OpenSpecy-aws-release-")
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  catalog <- .openspecy_library_catalog()

  for (type in catalog$type) {
    get_lib(type, path = tmp, quiet = TRUE)
    file <- file.path(tmp, paste0(type, ".rds"))
    expect_gt(unname(file.info(file)$size), 0)
    expect_match(digest::digest(file, algo = "sha256", file = TRUE),
                 "^[0-9a-f]{64}$")
    expect_no_error(load_lib(type, path = tmp))
  }

  full <- load_lib("derivative", path = tmp)$raman
  medoid <- load_lib("medoid_derivative", path = tmp)$raman
  model <- load_lib("model_derivative", path = tmp)$raman
  query <- filter_spec(full, 1L)

  expect_s3_class(match_spec(query, filter_spec(full, seq_len(10L)), top_n = 3L),
                  "data.table")
  medoid_query <- conform_spec(
    query, range = medoid$wavenumber, res = NULL, allow_na = FALSE
  )
  expect_s3_class(match_spec(medoid_query,
                             filter_spec(medoid, seq_len(10L)), top_n = 3L),
                  "data.table")
  expect_s3_class(match_spec(query, model), "data.table")
})
