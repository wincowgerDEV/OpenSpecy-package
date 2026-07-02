args_file <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE),
                                       value = TRUE)[1])
project <- if (length(args_file) && !is.na(args_file)) {
  normalizePath(file.path(dirname(args_file), ".."), winslash = "/")
} else {
  normalizePath(getwd(), winslash = "/")
}
if (!file.exists(file.path(project, "DESCRIPTION")))
  project <- normalizePath(getwd(), winslash = "/")

legacy_script <- Sys.getenv(
  "OPENSPECY_LEGACY_PIPELINE",
  unset = "H:/My Drive/Work/Projects/OpenSpecy/OpenSpecyDev/automated_steel_pipeline_Validation.R"
)
if (!file.exists(legacy_script)) {
  stop(
    "Legacy pipeline script was not found. Set OPENSPECY_LEGACY_PIPELINE ",
    "to automated_steel_pipeline_Validation.R so the benchmark can load the ",
    "exact analyze_features() implementation.",
    call. = FALSE
  )
}

cran_lib <- Sys.getenv("OPENSPECY_CRAN_LIB", unset = file.path(
  tempdir(), "openspecy-cran-lib"
))
cran_repos <- Sys.getenv("OPENSPECY_CRAN_REPOS",
                         unset = "https://cloud.r-project.org")
if (!dir.exists(file.path(cran_lib, "OpenSpecy"))) {
  dir.create(cran_lib, recursive = TRUE, showWarnings = FALSE)
  install.packages("OpenSpecy", lib = cran_lib, repos = cran_repos,
                   dependencies = FALSE)
}

copy_object <- function(x) unserialize(serialize(x, NULL))

load_legacy_pipeline <- function(path) {
  lines <- readLines(path, warn = FALSE)
  stop_line <- grep("^#Spectral Library ----", lines)
  if (!length(stop_line)) stop_line <- grep("^#Spectral Library", lines)
  if (!length(stop_line)) {
    stop("Could not find the end of the legacy function block in ", path,
         call. = FALSE)
  }
  env <- new.env(parent = .GlobalEnv)
  source_text <- paste(lines[seq_len(stop_line[1] - 1L)], collapse = "\n")
  source_text <- gsub(
    "dplyr::select(sample_name, material_class)",
    "dplyr::select(sample_name, dplyr::all_of(material_class))",
    source_text,
    fixed = TRUE
  )
  source_text <- gsub(
    "dplyr::select(proc_map$metadata, feature_id, material_class, max_cor_val)",
    "dplyr::select(proc_map$metadata, feature_id, dplyr::all_of(material_class), max_cor_val)",
    source_text,
    fixed = TRUE
  )
  source_text <- gsub(
    "group_by(material_class)",
    "group_by(.data[[material_class]])",
    source_text,
    fixed = TRUE
  )
  eval(parse(text = source_text), envir = env)
  if (!exists("analyze_features", envir = env, inherits = FALSE)) {
    stop("The legacy script did not define analyze_features()", call. = FALSE)
  }
  env
}

normalize_test_lib <- function(test_lib) {
  md <- data.table::as.data.table(test_lib$metadata)
  if (!"spectrum_type" %in% names(md) && "SpectrumType" %in% names(md)) {
    md$spectrum_type <- md$SpectrumType
  }
  if (!"polymer_class" %in% names(md)) {
    stop("test_lib metadata must include polymer_class for this benchmark",
         call. = FALSE)
  }
  test_lib$metadata <- md
  test_lib
}

as_legacy_open_specy <- function(x) {
  md <- data.table::copy(data.table::as.data.table(x$metadata))
  coords <- NULL
  if (all(c("x", "y") %in% names(md))) {
    coords <- md[, c("x", "y"), with = FALSE]
    md[, c("x", "y") := NULL]
  }
  as_OpenSpecy(
    x$wavenumber,
    spectra = as.data.frame(x$spectra, check.names = FALSE),
    metadata = md,
    coords = coords
  )
}

normalize_csv <- function(path) {
  out <- data.table::fread(path)
  numeric_cols <- names(out)[vapply(out, is.numeric, logical(1))]
  for (col in numeric_cols) {
    data.table::set(out, j = col, value = signif(out[[col]], 12))
  }
  out
}

normalize_os <- function(path) {
  x <- readRDS(path)
  md <- data.table::as.data.table(x$metadata)
  # Constructor provenance changes across CRAN/dev sessions; compare analysis
  # payloads rather than session hashes.
  drop_cols <- intersect(c("session_id", "file_id"), names(md))
  if (length(drop_cols)) md[, (drop_cols) := NULL]
  numeric_cols <- names(md)[vapply(md, is.numeric, logical(1))]
  for (col in numeric_cols) {
    data.table::set(md, j = col, value = signif(md[[col]], 10))
  }
  md <- as.data.frame(md, stringsAsFactors = FALSE)
  row.names(md) <- NULL
  spectra <- signif(as.matrix(x$spectra), 10)
  dimnames(spectra) <- NULL
  list(wavenumber = unname(signif(x$wavenumber, 10)),
       spectra = spectra,
       spectra_names = colnames(x$spectra),
       metadata = md)
}

compare_artifact <- function(name, old_dir, new_dir, reader) {
  old <- reader(file.path(old_dir, name))
  new <- reader(file.path(new_dir, name))
  equal <- identical(old, new)
  data.frame(file = name, identical = equal, stringsAsFactors = FALSE) |>
    print(row.names = FALSE)
  if (!equal) {
    if (is.list(old) && is.list(new) &&
        all(c("metadata") %in% names(old)) &&
        all(c("metadata") %in% names(new))) {
      message("Old metadata names: ", paste(names(old$metadata), collapse = ", "))
      message("New metadata names: ", paste(names(new$metadata), collapse = ", "))
    }
    diff <- all.equal(old, new, check.attributes = FALSE)
    print(diff)
    stop(name, " differed between legacy analyze_features() and ",
         "automate_particle_analysis()", call. = FALSE)
  }
  invisible(TRUE)
}

expect_nonempty_file <- function(path) {
  ok <- file.exists(path) && file.info(path)$size > 0
  data.frame(file = basename(path), nonempty = ok, stringsAsFactors = FALSE) |>
    print(row.names = FALSE)
  if (!ok) stop("Expected non-empty file: ", path, call. = FALSE)
  invisible(TRUE)
}

bench_dir <- Sys.getenv("OPENSPECY_APA_BENCH_DIR", unset = "")
if (!nzchar(bench_dir)) bench_dir <- tempfile("apa-legacy-exports-")
old_dir <- file.path(bench_dir, "legacy")
new_dir <- file.path(bench_dir, "current")
if (dir.exists(old_dir)) unlink(old_dir, recursive = TRUE)
if (dir.exists(new_dir)) unlink(new_dir, recursive = TRUE)
dir.create(old_dir, recursive = TRUE)
dir.create(new_dir, recursive = TRUE)

old_libpaths <- .libPaths()
.libPaths(c(cran_lib, old_libpaths))
legacy_env <- load_legacy_pipeline(legacy_script)
message("Legacy OpenSpecy version: ", as.character(packageVersion("OpenSpecy")))

load(file.path(project, "data", "test_lib.RData"))
test_lib <- normalize_test_lib(test_lib)
test_lib <- as_legacy_open_specy(test_lib)
lib <- filter_spec(test_lib, test_lib$metadata$spectrum_type == "FTIR")

tiny_map <- read_extdata("CA_tiny_map.zip") |> read_any()
legacy_input <- file.path(old_dir, "tiny_map.rds")
write_spec(tiny_map, file = legacy_input)

legacy_lib <- copy_object(lib)
current_lib <- copy_object(lib)

set.seed(42)
legacy_env$analyze_features(
  files = legacy_input,
  lib = legacy_lib,
  img = NULL,
  spectral_smooth = TRUE,
  close = FALSE,
  adj_map_baseline = FALSE,
  sn_threshold_min = 0.01,
  sn_threshold_max = Inf,
  cor_threshold = 0.7,
  area_threshold = 1,
  label_unknown = FALSE,
  remove_nonplastic = FALSE,
  remove_unknown = FALSE,
  pixel_length = 1,
  metric = "sig_times_noise",
  abs = FALSE,
  particle_id_strategy = "collapse",
  material_class = "polymer_class",
  vote_count = 10,
  collapse_function = mean,
  k = 1,
  k_weighting = "mean",
  wd = old_dir,
  types = c("particle_image", "particle_details", "particle_heatmap",
            "spectra_processed", "cor_heatmap", "particle_summary",
            "particle_heatmap_thresholded"),
  by = c("sample", "all"),
  width = 1000,
  height = 1000,
  units = "px"
)

if ("package:OpenSpecy" %in% search()) {
  detach("package:OpenSpecy", unload = TRUE, character.only = TRUE)
}
if ("OpenSpecy" %in% loadedNamespaces()) {
  try(unloadNamespace("OpenSpecy"), silent = TRUE)
}
.libPaths(old_libpaths)

if (!requireNamespace("pkgload", quietly = TRUE)) {
  stop("Package 'pkgload' is required for the current-package benchmark",
       call. = FALSE)
}
pkgload::load_all(project, export_all = FALSE, quiet = TRUE)
message("Current OpenSpecy version: ", as.character(packageVersion("OpenSpecy")))

set.seed(42)
current_result <- automate_particle_analysis(
  legacy_input,
  current_lib,
  output_dir = new_dir,
  material_col = "polymer_class",
  spectral_smooth = TRUE,
  close = FALSE,
  sn_threshold_min = 0.01,
  sn_threshold_max = Inf,
  cor_threshold = 0.7,
  area_threshold = 1,
  label_unknown = FALSE,
  remove_materials = NULL,
  remove_unknown = FALSE,
  pixel_length = 1,
  metric = "sig_times_noise",
  abs = FALSE,
  particle_id_strategy = "collapse",
  collapse_function = mean,
  outputs = c("details", "summary", "processed", "particle_image"),
  origins = list(x = 0, y = 0)
)

stopifnot(is.list(current_result))

csv_files <- c("particle_details_tiny_map.csv",
               "particle_summary_tiny_map.csv",
               "particle_details_all.csv",
               "particle_summary_all.csv")
rds_files <- c("particles_tiny_map.rds")

invisible(lapply(csv_files, compare_artifact, old_dir = old_dir,
                 new_dir = new_dir, reader = normalize_csv))
invisible(lapply(rds_files, compare_artifact, old_dir = old_dir,
                 new_dir = new_dir, reader = normalize_os))

expect_nonempty_file(file.path(old_dir, "particle_image_tiny_map.png"))
expect_nonempty_file(file.path(new_dir, "particle_image_tiny_map.png"))
expect_nonempty_file(file.path(old_dir, "particle_heatmap_tiny_map.png"))
expect_nonempty_file(file.path(old_dir, "cor_heatmap_tiny_map.png"))
expect_nonempty_file(file.path(old_dir,
                               "particle_heatmap_thresholdedtiny_map.jpg"))

cat("Legacy/current automated particle analysis data exports are identical.\n")
cat("Legacy-only plot exports were generated and current particle_image exists.\n")
cat("Compared outputs in: ", bench_dir, "\n", sep = "")
