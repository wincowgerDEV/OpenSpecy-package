data("raman_hdpe")

test_that("spec_res() gives correct output", {
  spec_res(raman_hdpe$wavenumber) |> round(2) |> expect_equal(2.54)
})
