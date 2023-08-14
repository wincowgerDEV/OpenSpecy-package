# Create temp dir for testthat
tmp <- file.path(tempdir(), "OpenSpecy-testthat")
dir.create(tmp, showWarnings = F)

test_that("stop if OSF not reachable", {
  skip_on_cran()
  skip_if_not(is.null(curl::nslookup("api.osf.io", error = F)),
              message = "OSF is online")

  expect_error(get_lib(type = "test", path = tmp))
})

test_that("get_lib() downloads test library", {
  skip_on_cran()
  skip_if_offline(host = "api.osf.io")

  expect_message(
    expect_output(get_lib(type = "test", path = tmp))
    )
})

test_that("check_lib() finds test library", {
  skip_on_cran()
  skip_if_offline(host = "api.osf.io")

  expect_silent(check_lib(type = "test", path = tmp))
  expect_warning(check_lib(type = c("raw", "derivative", "nobaseline"),
                           path = tmp))
})

test_that("get_lib() downloads complete library", {
  skip_on_cran()
  skip_if_offline(host = "api.osf.io")
  skip_if_not(testthat:::on_ci(), "Not on CI")

  expect_message(
    expect_output(get_lib(type = c("derivative", "nobaseline"), path = tmp))
    )
})

test_that("check_lib() finds complete library", {
  skip_on_cran()
  skip_if_offline(host = "api.osf.io")
  skip_if_not(testthat:::on_ci(), "Not on CI")

  expect_silent(check_lib(type = c("derivative", "nobaseline"), path = tmp))
  expect_warning(check_lib(type = "raw", path = tmp))
})

test_that("load_lib() works as expected", {
  skip_on_cran()
  skip_if_offline(host = "api.osf.io")

  expect_silent(tl <- load_lib(type = "test", path = tmp))
  expect_type(tl, "list")
  expect_s3_class(tl, "OpenSpecy")
  expect_identical(tl, test_lib, ignore_attr = T)
})

# Tidy up
unlink(tmp, recursive = T)
