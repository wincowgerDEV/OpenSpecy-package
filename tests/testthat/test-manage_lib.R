data("test_lib")

# Create temp dir for testthat
tmp <- file.path(tempdir(), "OpenSpecy-testthat")
dir.create(tmp, showWarnings = F)

test_that("get_lib() downloads test library", {
  expect_message(
    expect_output(get_lib(which = "test", type = c("metadata", "library"),
                          path = tmp))
  )
})

test_that("check_lib() finds test library", {
  expect_silent(check_lib(which = "test", type = c("metadata", "library"),
                          path = tmp))
  expect_warning(check_lib(which = "test", type = c("peaks"),
                           path = tmp))
})

test_that("load_lib() works as expected", {
  expect_silent(tl <- load_lib(which = "test", type = c("metadata", "library"),
                               path = tmp))
  expect_type(tl, "list")
  expect_identical(tl, test_lib, ignore_attr = T)
})

# Tidy up
unlink(tmp, recursive = T)
