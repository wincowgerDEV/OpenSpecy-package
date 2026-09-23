devtools::load_all(quiet = TRUE)

path <- "C:/Users/winco/OneDrive/Documents/EWG/SilverTest/EWG_0.2umFilter_10-100-1000-10000_Region2.h5"
source <- open_specs(path, cache_dir = file.path(tempdir(), "openspecy-h5-probe"))
index <- OpenSpecy:::.filespec_index(source)
source$view <- index$index[index$col <= sort(unique(index$col))[[4L]]]

library <- readRDS(
  "../reference-library-build-2.0.0/releases/2c1b3ce60210/medoid_derivative.rds"
)$raman

started <- proc.time()[["elapsed"]]
result <- automate_particle_analysis(
  source,
  library = library,
  material_col = "material_class",
  particle_id_strategy = "all_cell_id",
  spectral_smooth = TRUE,
  sn_threshold_min = 2,
  sn_threshold_max = Inf,
  cor_threshold = 0.6,
  area_threshold = 1,
  label_unknown = TRUE,
  remove_unknown = FALSE,
  pixel_length = 1,
  metric = "run_sig_over_noise",
  collapse_function = mean,
  outputs = c("details", "summary", "processed"),
  origins = list(x = 0, y = 0)
)

cat("elapsed_seconds:", proc.time()[["elapsed"]] - started, "\n")
cat("input_spectra:", length(source$view), "\n")
cat("samples:", paste(names(result$samples), collapse = ", "), "\n")
cat("particles:", nrow(result$particle_details_all_csv), "\n")
cat("summary_rows:", nrow(result$particle_summary_all_csv), "\n")
