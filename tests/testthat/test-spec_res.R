data("raman_hdpe")

test_that("spec_res() gives correct output", {
  expect_equal(round(spec_res(raman_hdpe$wavenumber), 3), 3.005)
})
