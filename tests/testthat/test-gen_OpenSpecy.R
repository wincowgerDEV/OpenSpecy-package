library(data.table)
data("raman_hdpe")

test_that("print() and summary() work as expected", {
  print(raman_hdpe) |> expect_output(
    paste0("wavenumber.*intensity.*308.221.*",
           "48.*metadata.*spectrum_identity.*HDPE"))
  summary(raman_hdpe) |> expect_output("Length.*964.*spectra.*26")
})

test_that("head returns the first few lines of the OpenSpecy object", {
  head <- head(raman_hdpe)

  nrow(head) |> expect_equal(6)
  head |> expect_equal(head(data.table(wavenumber = raman_hdpe$wavenumber,
                                       raman_hdpe$spectra)))
})

test_that("ploting works without errors", {
  plot(raman_hdpe) |> expect_silent()
})
