data("raman_hdpe")

raman_hdpe$spectra$intensity2 <- raman_hdpe$spectra$intensity * 2
raman_hdpe$metadata <- rbind(raman_hdpe$metadata, raman_hdpe$metadata)

test_that("adj_intens() handles input errors correctly", {
  adj_intens("abc") |> expect_error()
})

test_that("adj_intens() works as expected", {
  adj <- adj_intens(raman_hdpe) |> expect_silent()
  adj_intens(raman_hdpe, type = "reflectance") |> expect_silent()
  adj_intens(raman_hdpe, type = "transmittance") |> expect_silent()
  adj_intens(raman_hdpe, type = "transmission") |> expect_error()
  
  adj_intens(raman_hdpe, type = "reflectance", log_exp = "log") |> expect_silent()
  adj_intens(raman_hdpe, type = "transmittance", log_exp = "exp") |> expect_silent()
  
  expect_equal(
    cor(raman_hdpe$spectra$intensity, adj$spectra$intensity), 1,
    ignore_attr = F)
  expect_equal(
    cor(raman_hdpe$spectra$intensity, adj$spectra$intensity2), 1,
    ignore_attr = F)

  expect_s3_class(adj, "OpenSpecy")
  expect_true(check_OpenSpecy(adj))

  expect_equal(nrow(adj$spectra), nrow(raman_hdpe$spectra))
  expect_equal(adj$wavenumber, raman_hdpe$wavenumber)
  expect_equal(adj$spectra |> range(), c(0, 1))
})
