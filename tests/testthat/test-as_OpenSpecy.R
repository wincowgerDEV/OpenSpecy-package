data("raman_hdpe")
df <- read_extdata("raman_hdpe.csv") |> read.csv()

test_that("as_OpenSpecy() handles errors correctly", {
  as_OpenSpecy(df$wavenumber, as.data.frame(df$intensity)) |> expect_silent()
  as_OpenSpecy(df$wavenumber, df$intensity) |> expect_error()
  as_OpenSpecy(df$wavenumber) |> expect_error()
  as_OpenSpecy(df$wavenumber, as.data.frame(df$intensity[-1])) |> expect_error()
  as_OpenSpecy(df$wavenumber, data.table(intensity = df$intensity,
                                         intensity = df$intensity)) |>
    expect_error()

  as_OpenSpecy(df, metadata = list(file_name = "test", 
                                   file_name = "test", 
                                   fun_val = "t", 
                                   check = "c",
                                   check = "a",
                                   check = "a")) |>
      check_OpenSpecy() |>
      expect_true() |>
      expect_message() 
  
  as_OpenSpecy(data.frame(x = df$wavenumber, abs = df$intensity)) |>
    expect_message()
  as_OpenSpecy(data.frame(wav = df$wavenumber, y = df$intensity)) |>
    expect_message()

  amb <- df
  names(amb) <- c("a", "b")
  as_OpenSpecy(amb) |> expect_message() |> expect_message()

  as_OpenSpecy(df$wavenumber, as.data.frame(df$intensity), coords = "") |>
    expect_error()
  as_OpenSpecy(df$wavenumber, as.data.frame(df$intensity), coords = df) |>
    expect_error()
  as_OpenSpecy(df$wavenumber, as.data.frame(df$intensity), metadata = "") |>
    expect_error()
})

test_that("as_OpenSpecy() generates OpenSpecy objects", {
  as_OpenSpecy(df) |> expect_silent()
  as_OpenSpecy(raman_hdpe, session_id = T) |> expect_silent()

  osf <- as_OpenSpecy(df) |> expect_silent()
  ost <- data.table(df) |> as_OpenSpecy() |> expect_silent()
  osl <- list(df$wavenumber, df[2]) |> as_OpenSpecy() |> expect_silent()
  OpenSpecy(osf) |> expect_silent()

  expect_s3_class(osf, "OpenSpecy")
  expect_s3_class(ost, "OpenSpecy")
  expect_s3_class(osl, "OpenSpecy")

  expect_true(check_OpenSpecy(osf))
  expect_true(check_OpenSpecy(ost))
  expect_true(check_OpenSpecy(osl))

  expect_true(is_OpenSpecy(osf))
  expect_true(is_OpenSpecy(ost))
  expect_true(is_OpenSpecy(osl))
  expect_false(is_OpenSpecy(df))

  expect_equal(names(osf), c("wavenumber", "spectra", "metadata"))
  expect_equal(names(ost), c("wavenumber", "spectra", "metadata"))
  expect_equal(names(osl), c("wavenumber", "spectra", "metadata"))

  expect_equal(OpenSpecy(df), OpenSpecy(osf))
  expect_equal(ost$spectra, osf$spectra)
  expect_equal(ost$wavenumber, osf$wavenumber)
  expect_equal(ost$metadata, osf$metadata)
  expect_equal(osf$metadata$col_id, colnames(osf$spectra))
})

test_that("as_OpenSpecy() upgrades old data.table spectra", {
  os <- as_OpenSpecy(df)
  old <- os
  old$spectra <- data.table::as.data.table(old$spectra)

  expect_silent(upgraded <- as_OpenSpecy(old))
  expect_true(is.matrix(upgraded$spectra))
  expect_equal(upgraded$spectra, os$spectra)
})

test_that("check_OpenSpecy() work as expected", {
  expect_invalid <- function(object, pattern) {
    emitted <- character()
    valid <- withCallingHandlers(
      check_OpenSpecy(object),
      warning = function(warning) {
        emitted <<- c(emitted, conditionMessage(warning))
        invokeRestart("muffleWarning")
      }
    )
    expect_false(valid)
    expect_true(
      any(grepl(pattern, emitted, ignore.case = TRUE)),
      info = paste(emitted, collapse = " | ")
    )
  }

  os <- as_OpenSpecy(df)
  check_OpenSpecy(os) |> expect_true()
  expect_invalid(df, "not of class")

  osna <- osd <- osv <- osn <- oss <- ost <- osl <- os

  osv$wavenumber <- list(osv$wavenumber)
  expect_invalid(osv, "length of wavenumber")

  names(osn) <- 1:3
  expect_invalid(osn, "names of the object components")

  osna$spectra[2:nrow(osna$spectra), "intensity"] <- NA
  expect_invalid(osna, "one or fewer non NA values")
  
  osna$spectra[, "intensity"] <- NA
  expect_invalid(osna, "one or fewer non NA values")
  
  oss$wavenumber <- sample(oss$wavenumber)
  expect_invalid(oss, "continuous sequence")

  class(ost$metadata) <- class(ost$spectra) <- "data.frame"
  expect_invalid(ost, "metadata are not")

  osl$metadata <- rbind(osl$metadata, osl$metadata)
  osl$spectra <- osl$spectra[-1]

  expect_invalid(osl, "number of columns in spectra")
  
  #dup wavenumbers
  osd$wavenumber <- rep(300, length.out = length(osd$wavenumber))

  expect_invalid(osd, "unqiue values")
  
})

test_that("'OpenSpecy' objects are read correctly", {
  os <- as_OpenSpecy(df)

  expect_true(check_OpenSpecy(os))

  expect_equal(range(os$wavenumber) |> round(2), c(301.04, 3198.12))
  expect_equal(range(os$spectra) |> round(2), c(26, 816))
  expect_length(os$wavenumber, 964)
  expect_equal(os$metadata$x, 1)
  expect_equal(os$metadata$y, 1)
  expect_equal(os$metadata$license, "CC BY-NC")
})

test_that("'OpenSpecy' objects are transcribed to and from 'hyperSpec' objects", {
  os <- as_OpenSpecy(df)
  hyper <- as_hyperSpec(os)
  expect_s4_class(hyper, "hyperSpec")

  openhyper <- as_OpenSpecy(hyper)

  expect_true(is_OpenSpecy(openhyper))
  expect_true(check_OpenSpecy(openhyper))

  expect_equal(openhyper$wavenumber, hyper@wavelength)
  expect_equal(openhyper$spectra[, 1], unname(t(hyper$spc)[,1]))
})

