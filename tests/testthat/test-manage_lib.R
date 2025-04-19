# Create temp dir for testthat
tmp <- file.path(tempdir(), "OpenSpecy-testthat")
dir.create(tmp, showWarnings = F)


test_that("check_lib() finds complete library", {
  skip_on_cran()
  skip_if_offline(host = "api.osf.io")
  skip_if_not(testthat:::on_ci(), "Not on CI")
  
  get_lib(type = c("derivative"), path = tmp, aws = TRUE, revision = "DtlNCNhOGdtCJqRCN3F8jRs732t_yZLM")
  check_lib(type = c("derivative"), path = tmp) |>
      expect_silent()
  get_lib(type = c("nobaseline"), path = tmp, aws = TRUE)
  check_lib(type = c("nobaseline"), path = tmp) |>
      expect_silent()
  get_lib(type = c("medoid_derivative"), path = tmp)
  check_lib(type = c("medoid_derivative"), path = tmp) |>
    expect_silent()
  get_lib(type = c("medoid_nobaseline"), path = tmp, revision = 1)
  check_lib(type = c("medoid_nobaseline"), path = tmp) |>
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
