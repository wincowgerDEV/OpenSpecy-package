test_that("AWS release metadata covers every public library type", {
  release <- .openspecy_library_release()
  expected <- c(
    "derivative", "nobaseline", "raw", "medoid_derivative",
    "medoid_nobaseline", "model_derivative", "model_nobaseline"
  )

  expect_identical(release$type, expected)
  expect_identical(release$filename, paste0(expected, ".rds"))
  expect_true(all(nzchar(release$version_id)))
  expect_true(all(grepl("^[0-9a-f]{64}$", release$sha256)))
  expect_true(all(release$bytes > 0))
  expect_false("aws" %in% names(formals(get_lib)))
})

test_that("get_lib() builds AWS URLs including raw and S3 version IDs", {
  tmp <- tempfile("OpenSpecy-get-lib-")
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  urls <- character()

  local_mocked_bindings(
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
  expect_identical(
    urls,
    c(
      "https://d2jrxerjcsjhs7.cloudfront.net/raw.rds?versionId=test-version",
      "https://d2jrxerjcsjhs7.cloudfront.net/derivative.rds?versionId=test-version"
    )
  )
  expect_true(all(file.exists(file.path(tmp, c("raw.rds", "derivative.rds")))))
})

test_that("named revisions pin each requested library independently", {
  revisions <- c(raw = "raw-version", derivative = "derivative-version")
  expect_identical(.library_revision(revisions, "raw"), "raw-version")
  expect_identical(
    .library_revision(revisions, "derivative"), "derivative-version"
  )
  expect_error(.library_revision(unname(revisions), "raw"), "must be named")
})

test_that("all pinned AWS libraries download, verify, load, and match", {
  skip_on_cran()
  skip_if_not(
    identical(Sys.getenv("OPENSPECY_RUN_AWS_LIBRARY_TESTS"), "true"),
    "Set OPENSPECY_RUN_AWS_LIBRARY_TESTS=true for the large AWS integration"
  )
  skip_if_offline(host = "d2jrxerjcsjhs7.cloudfront.net")

  tmp <- tempfile("OpenSpecy-aws-release-")
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  release <- .openspecy_library_release()
  revisions <- setNames(release$version_id, release$type)

  for (type in release$type) {
    get_lib(type, path = tmp, revision = revisions[[type]], quiet = TRUE)
    file <- file.path(tmp, paste0(type, ".rds"))
    row <- release[release$type == type, , drop = FALSE]
    expect_identical(unname(file.info(file)$size), row$bytes)
    expect_identical(
      digest::digest(file, algo = "sha256", file = TRUE),
      row$sha256
    )
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
