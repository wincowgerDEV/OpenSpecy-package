map <- read_extdata("testdata_zipped.zip") |> read_any()

spectra <- lapply(c(read_extdata("raman_hdpe.csv"), read_extdata("raman_hdpe.csv"), read_extdata("raman_hdpe.csv")), read_text)

spectra2 <- c_spec(spectra)
