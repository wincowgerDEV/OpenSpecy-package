data("raman_hdpe")
raman_hdpe$spectra$intensity2 <- raman_hdpe$spectra$intensity

test_that("adj_intens() works as expected", {
  expect_silent(adj <- adj_intens(raman_hdpe, type = "reflectance"))
  expect_silent(adj <- adj_intens(raman_hdpe, type = "transmittance"))
  expect_silent(adj <- adj_intens(raman_hdpe))
  expect_equal(
    cor(raman_hdpe$spectra$intensity,
        adj_intens(raman_hdpe)$spectra$intensity) |> as.numeric(),
    1, ignore_attr = F)
  expect_s3_class(adj, "OpenSpecy")
  expect_equal(nrow(adj$spectra), nrow(raman_hdpe$spectra))
  expect_equal(adj$wavenumber, raman_hdpe$wavenumber)
  expect_equal(adj_intens(raman_hdpe, make_rel = T)$spectra |> range(),
               c(0, 1))
})

test_that("adj_intens() runs without error", {
  expect_no_error(adj_intens(raman_hdpe, type = "reflectance"))
  expect_no_error(adj_intens(raman_hdpe, type = "transmittance"))
  expect_no_error(adj_intens(raman_hdpe))
})

