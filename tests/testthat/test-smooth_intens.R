data("raman_hdpe")

test_that("smooth_intens() works as expected", {
  expect_silent(smt <- smooth_intens(raman_hdpe, p = 3))
  expect_identical(smt, smooth_intens(raman_hdpe$wavenumber,
                                         raman_hdpe$intensity))
  expect_identical(smt, smooth_intens(intensity ~ wavenumber, raman_hdpe))
  expect_equal(as.numeric(
    round(cor(smt[2], smooth_intens(raman_hdpe, p = 1)[2]), 4)
    ), 0.9756, ignore_attr = F)
  expect_s3_class(smt, "data.frame")
  expect_equal(names(smt), c("wavenumber", "intensity"))
  expect_equal(nrow(smt), nrow(raman_hdpe))
  expect_equal(smt[1], raman_hdpe[1])
  expect_equal(range(smt[2]), c(0, 1))
})
