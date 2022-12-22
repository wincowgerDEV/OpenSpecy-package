library(data.table)

df <- read_extdata("raman_hdpe.csv") |> read.csv()

test_that("as_OpenSpecy() generates OpenSpecy objects", {
  expect_silent(OpenSpecy(df))

  expect_silent(osf <- as_OpenSpecy(df))
  expect_silent(ost <- data.table(df) |> as_OpenSpecy())
  expect_silent(osl <- list(df$Wavelength, df[2]) |> as_OpenSpecy())
  expect_silent(OpenSpecy(osf))

  expect_s3_class(osf, "OpenSpecy")
  expect_s3_class(ost, "OpenSpecy")
  expect_s3_class(osl, "OpenSpecy")

  expect_equal(names(osf), c("wavenumber", "spectra", "metadata"))
  expect_equal(names(ost), c("wavenumber", "spectra", "metadata"))
  expect_equal(names(osl), c("wavenumber", "spectra", "metadata"))

  expect_equal(OpenSpecy(df), OpenSpecy(osf))
  expect_equal(ost$spectra, osf$spectra)
  expect_equal(ost$wavenumber, osf$wavenumber)
  expect_equal(ost$metadata, osf$metadata)

  expect_true(is_OpenSpecy(osf))
  expect_true(is_OpenSpecy(ost))
  expect_true(is_OpenSpecy(osl))
  expect_false(is_OpenSpecy(df))
})

test_that("as_OpenSpecy() handles errors correctly", {
  expect_silent(as_OpenSpecy(df$Wavelength,
                             as.data.frame(df$Absorbance)))
  expect_error(as_OpenSpecy(df$Wavelength,
                            df$Absorbance))

  expect_error(as_OpenSpecy(df$Wavelength))
  expect_error(as_OpenSpecy(as.character(df$Wavelength),
                            as.data.frame(df$Absorbance)))
  expect_error(as_OpenSpecy(df$Wavelength,
                            as.data.frame(df$Absorbance[-1])))

  expect_warning(as_OpenSpecy(data.frame(x = df$Wavelength,
                                         abs = df$Absorbance)))
  expect_warning(as_OpenSpecy(data.frame(wav = df$Wavelength,
                                         y = df$Absorbance)))

  expect_error(as_OpenSpecy(df$Wavelength,
                            as.data.frame(df$Absorbance),
                            coords = ""))
  expect_error(as_OpenSpecy(df$Wavelength,
                            as.data.frame(df$Absorbance),
                            coords = df))
  expect_error(as_OpenSpecy(df$Wavelength,
                            as.data.frame(df$Absorbance),
                            metadata = ""))
})

test_that("OpenSpecy objects are read correctly", {
  os <- as_OpenSpecy(df)

  expect_equal(range(os$wavenumber) |> round(2), c(150.92, 2998.49))
  expect_equal(range(os$spectra) |> round(2), c(3264.21, 41238.90))
  expect_length(os$wavenumber, 1095)
  expect_equal(os$metadata$x, 1)
  expect_equal(os$metadata$y, 1)
  expect_equal(os$metadata$license, "CC BY-NC")
})
