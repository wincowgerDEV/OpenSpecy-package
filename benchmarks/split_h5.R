# Native H5 region-copy performance and RDS parity probe.

devtools::load_all()

directory <- tempfile("split-h5-benchmark-")
dir.create(directory)
on.exit(unlink(directory, recursive = TRUE, force = TRUE), add = TRUE)
file <- file.path(directory, "map.h5")
bands <- 256L
rows <- 64L
cols <- 64L

h5 <- hdf5r::H5File$new(file, mode = "w")
info <- h5$create_group("FileInfo")
xml <- paste0(
  "<VAR TYPE=\"System.Double\" NAME=\"m_StartFrequency\">100</VAR>",
  "<VAR TYPE=\"System.Double\" NAME=\"m_EndFrequency\">400</VAR>",
  "<VAR TYPE=\"System.Int32\" NAME=\"SpectrumPoints\">", bands,
  "</VAR>"
)
info[["MetaData"]] <- as.integer(charToRaw(xml))
regions <- h5$create_group("Regions")
values <- array(sin(seq_len(bands * rows * cols) / 100),
                dim = c(bands, rows, cols))
for (i in 1:2) {
  region <- regions$create_group(paste0("Region", i))
  region[["Dataset"]] <- values + i
}
h5$close_all()
rm(values)
invisible(gc(verbose = FALSE))

h5_time <- system.time(h5_paths <- split_h5(
  file, output_dir = file.path(directory, "h5")
))[["elapsed"]]
rds_time <- system.time(rds_paths <- split_h5(
  file, output_dir = file.path(directory, "rds"), format = "rds"
))[["elapsed"]]

for (region in names(h5_paths)) {
  h5_piece <- read_h5(h5_paths[[region]], read_visual = FALSE)
  rds_piece <- readRDS(rds_paths[[region]])
  stopifnot(isTRUE(all.equal(h5_piece$spectra, rds_piece$spectra,
                            check.attributes = FALSE)))
}

cat(sprintf("native H5: %.3fs; RDS: %.3fs; RDS/H5 ratio: %.2fx\n",
            h5_time, rds_time, rds_time / max(h5_time, .Machine$double.eps)))
