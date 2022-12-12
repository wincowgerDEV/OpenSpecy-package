data("raman_hdpe")

test_that("adj_intens() works as expected", {
  expect_silent(adj <- adj_intens(raman_hdpe))
  expect_equal(
    cor(raman_hdpe$spectra$intensity,
        adj_intens(raman_hdpe)$spectra$intensity) %>% as.numeric(),
    1, ignore_attr = F)
  expect_s3_class(adj, "OpenSpecy")
  expect_equal(nrow(adj$spectra), nrow(raman_hdpe$spectra))
  expect_equal(adj$wavenumber, raman_hdpe$wavenumber)
  expect_equal(adj_intens(raman_hdpe, make_rel = T)$spectra %>% range(),
               c(0, 1))
})
