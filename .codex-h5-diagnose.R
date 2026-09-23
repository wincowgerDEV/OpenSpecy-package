devtools::load_all(quiet = TRUE)

path <- "C:/Users/winco/OneDrive/Documents/EWG/SilverTest/EWG_0.2umFilter_10-100-1000-10000_Region2.h5"
cache <- file.path(tempdir(), "openspecy-h5-diagnose-cache")
started <- proc.time()[["elapsed"]]
specs <- open_specs(path, cache_dir = cache)
elapsed <- proc.time()[["elapsed"]] - started
index <- OpenSpecy:::.filespec_index(specs)

cat("class:", paste(class(specs), collapse = ", "), "\n")
cat("backend:", specs$source$backend, "\n")
cat("bands:", length(specs$source$axis), "\n")
cat("spectra:", nrow(index), "\n")
cat("regions:", paste(unique(index$region), collapse = ", "), "\n")
cat("grid rows x cols:", length(unique(index$row)), "x",
    length(unique(index$col)), "\n")
cat("open seconds:", elapsed, "\n")
cat("eager spectra bytes:",
    format(as.double(length(specs$source$axis)) * nrow(index) * 8,
           scientific = FALSE), "\n")
cat("index MB:", format(as.numeric(object.size(index)) / 1024^2,
                         digits = 4), "\n")
cat("layout:\n")
str(specs$source$layout, max.level = 2)

bands <- which(
  (specs$source$axis >= 750 & specs$source$axis <= 2200) |
    (specs$source$axis >= 2420 & specs$source$axis <= 4000)
)
cat("S/N bands:", length(bands), "\n")
cat("bounded chunk:",
    OpenSpecy:::.filespec_bounded_chunk_size(length(bands), 8192L), "\n")

probe_rows <- seq_len(min(64L, nrow(index)))
started <- proc.time()[["elapsed"]]
probe <- OpenSpecy:::.filespec_read_values(specs, index = probe_rows,
                                           bands = bands)
cat("64-row probe dims:", paste(dim(probe$spectra), collapse = " x "), "\n")
cat("64-row probe seconds:", proc.time()[["elapsed"]] - started, "\n")
