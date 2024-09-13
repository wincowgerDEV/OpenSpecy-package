# Create temp dir for testthat
tmp <- file.path(tempdir(), "OpenSpecy-testthat")
dir.create(tmp, showWarnings = F)

#test_that("stop if OSF not reachable", {
#  skip_on_cran()
  #skip_if_not(is.null(curl::nslookup("api.osf.io", error = F)),
  #            message = "OSF is online") #Curl doesn't work nicely with shinylive

#  get_lib(type = "test", path = tmp) |>
#    expect_error()
#})



test_that("check_lib() finds complete library", {
  skip_on_cran()
  skip_if_offline(host = "api.osf.io")
  skip_if_not(testthat:::on_ci(), "Not on CI")

  check_lib(type = c("derivative", "nobaseline"), path = tmp) |>
    expect_silent()
  check_lib(type = "raw", path = tmp) |>
    expect_warning()
})

test_that("load_lib() works with complete library", {
  skip_on_cran()
  skip_if_offline(host = "api.osf.io")
  skip_if_not(testthat:::on_ci(), "Not on CI")

  tl <- load_lib(type = "derivative", path = tmp) |>
    expect_silent()
  expect_type(tl, "list")
  expect_s3_class(tl, "OpenSpecy")
  expect_true(check_OpenSpecy(tl))
})

# Tidy up
unlink(tmp, recursive = T)
