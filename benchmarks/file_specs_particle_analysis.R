# Opt-in benchmark for experimental file-backed H5/ENVI workflows.
#
# Run from the package root with, for example:
# OPENSPECY_RUN_FILE_SPECS_BENCHMARK=true Rscript \
#   benchmarks/file_specs_particle_analysis.R
#
# Optional external inputs:
#   OPENSPECY_H5_BENCH_FILE              large H5 source
#   OPENSPECY_ENVI_BENCH_FILE            ENVI .hdr/.dat/.img member
#   OPENSPECY_RDS_BENCH_FILE             eager Region 1 OpenSpecy oracle
#   OPENSPECY_PARTICLE_LIBRARY_RDS       OpenSpecy/Specs reference library
#   OPENSPECY_FILE_SPECS_BENCH_REGION    region used by the particle pipeline
#   OPENSPECY_FILE_SPECS_BENCH_REPS      repetitions (default 3)
#   OPENSPECY_FILE_SPECS_BENCH_TOP_N     exact ranks for OpenSpecy libraries
#   OPENSPECY_FILE_SPECS_BENCH_PEAK      use optional peakRAM measurements
#   OPENSPECY_FILE_SPECS_BENCH_OUTPUT    new CSV path for results

if (requireNamespace("pkgload", quietly = TRUE)) {
  pkgload::load_all(".", export_all = TRUE)
} else {
  library(OpenSpecy)
}

.bench_flag <- function(name, default = FALSE) {
  value <- tolower(Sys.getenv(name, if (default) "true" else "false"))
  value %in% c("1", "true", "yes", "on")
}

.bench_number <- function(name, default, integer = FALSE) {
  value <- suppressWarnings(as.numeric(Sys.getenv(name, as.character(default))))
  if (length(value) != 1L || !is.finite(value) || value <= 0) {
    stop(name, " must be one positive number", call. = FALSE)
  }
  if (integer) as.integer(value) else value
}

.bench_measure_once <- function(label, fun, use_peak = FALSE) {
  gc()
  if (use_peak) {
    if (!requireNamespace("peakRAM", quietly = TRUE)) {
      stop("Install the optional 'peakRAM' package or set ",
           "OPENSPECY_FILE_SPECS_BENCH_PEAK=false", call. = FALSE)
    }
    measured <- peakRAM::peakRAM(invisible(fun()))
    return(data.frame(
      case = label,
      elapsed_seconds = measured$Elapsed_Time_sec[[1L]],
      allocated_mb = NA_real_,
      peak_ram_mb = measured$Peak_RAM_Used_MiB[[1L]]
    ))
  }

  profile <- tempfile("filespec-rprofmem-", fileext = ".out")
  on.exit(unlink(profile, force = TRUE), add = TRUE)
  utils::Rprofmem(profile, threshold = 1000)
  timing <- tryCatch(
    system.time(invisible(fun())),
    finally = utils::Rprofmem(NULL)
  )
  lines <- readLines(profile, warn = FALSE)
  allocations <- suppressWarnings(as.numeric(
    sub(" .*", "", grep("^[0-9]+", lines, value = TRUE))
  ))
  data.frame(
    case = label,
    elapsed_seconds = unname(timing[["elapsed"]]),
    allocated_mb = sum(allocations, na.rm = TRUE) / 1024^2,
    peak_ram_mb = NA_real_
  )
}

.bench_repeat <- function(label, fun, repetitions, use_peak = FALSE) {
  rows <- lapply(seq_len(repetitions), function(i) {
    message("Benchmarking ", label, " [", i, "/", repetitions, "]")
    row <- .bench_measure_once(label, fun, use_peak)
    row$iteration <- i
    row
  })
  do.call(rbind, rows)[, c("case", "iteration", "elapsed_seconds",
                           "allocated_mb", "peak_ram_mb")]
}

.bench_dir_bytes <- function(path) {
  files <- list.files(path, recursive = TRUE, full.names = TRUE,
                      all.files = TRUE, no.. = TRUE)
  if (!length(files)) return(0)
  sum(file.info(files)$size, na.rm = TRUE)
}

.bench_snapshot <- function(specs) {
  members <- specs$source$members
  data.frame(
    path = members$path,
    size = file.info(members$path)$size,
    mtime = as.numeric(file.info(members$path)$mtime),
    sha256 = vapply(members$path, digest::digest, character(1),
                    algo = "sha256", file = TRUE),
    stringsAsFactors = FALSE
  )
}

.bench_make_tiny_envi <- function(directory) {
  header <- file.path(directory, "tiny-particle.hdr")
  binary <- file.path(directory, "tiny-particle.dat")
  axis <- c(800, 1200, 2500, 3000)
  particle <- c(1, 3, 2, 4)
  writeLines(c(
    "ENVI", "samples = 4", "lines = 4", "bands = 4",
    "header offset = 0", "data type = 4", "interleave = bip",
    "byte order = 0",
    paste0("wavelength = {", paste(axis, collapse = ", "), "}")
  ), header)
  values <- unlist(lapply(0:3, function(row) {
    unlist(lapply(0:3, function(col) {
      if (row %in% 1:2 && col %in% 1:2) particle else rep(0, 4)
    }))
  }))
  connection <- file(binary, open = "wb")
  on.exit(close(connection), add = TRUE)
  writeBin(as.numeric(values), connection, size = 4L, endian = "little")
  library <- as_OpenSpecy(
    axis,
    matrix(particle, ncol = 1, dimnames = list(NULL, "particle")),
    metadata = data.frame(sample_name = "particle",
                          material_class = "polymer")
  )
  list(path = header, library = library)
}

.bench_region_view <- function(specs, region) {
  views <- split_spec(specs, by = "region")
  if (!nzchar(region)) return(views[[1L]])
  if (!region %in% names(views)) {
    stop("Requested benchmark region '", region, "' is unavailable; choose: ",
         paste(names(views), collapse = ", "), call. = FALSE)
  }
  views[[region]]
}

.bench_eager_subset <- function(x, positions) {
  out <- x
  out$spectra <- x$spectra[, positions, drop = FALSE]
  out$metadata <- data.table::copy(
    data.table::as.data.table(x$metadata)[positions]
  )
  out
}

.bench_particle_args <- function(library, tiny = FALSE) {
  lower_default <- if (tiny) 5 else 0.04
  maximum <- suppressWarnings(as.numeric(Sys.getenv(
    "OPENSPECY_FILE_SPECS_BENCH_SN_MAX", "Inf"
  )))
  area <- suppressWarnings(as.numeric(Sys.getenv(
    "OPENSPECY_FILE_SPECS_BENCH_AREA_MIN", if (tiny) "0" else "1"
  )))
  if (length(maximum) != 1L || is.na(maximum) || maximum <= 0) {
    stop("OPENSPECY_FILE_SPECS_BENCH_SN_MAX must be positive", call. = FALSE)
  }
  if (length(area) != 1L || !is.finite(area) || area < 0) {
    stop("OPENSPECY_FILE_SPECS_BENCH_AREA_MIN must be nonnegative",
         call. = FALSE)
  }
  list(
    library = library,
    particle_id_strategy = "collapse",
    spectral_smooth = FALSE,
    sn_threshold_min = .bench_number(
      "OPENSPECY_FILE_SPECS_BENCH_SN_MIN", lower_default
    ),
    sn_threshold_max = maximum,
    cor_threshold = .bench_number(
      "OPENSPECY_FILE_SPECS_BENCH_COR_MIN", 0.7
    ),
    top_n = .bench_number(
      "OPENSPECY_FILE_SPECS_BENCH_TOP_N", 1, integer = TRUE
    ),
    area_threshold = area,
    metric = if (tiny) "tot_sig" else Sys.getenv(
      "OPENSPECY_FILE_SPECS_BENCH_SN_METRIC", "sig_times_noise"
    ),
    collapse_function = mean,
    outputs = c("details", "summary", "processed", "particle_image",
                "sn_histogram", "cor_histogram"),
    process_args = list(smooth_intens = FALSE, make_rel = TRUE)
  )
}

.bench_compare_particle_results <- function(file_backed, eager,
                                             tolerance = 1e-10) {
  shared <- intersect(names(file_backed$particle_details_all_csv),
                      names(eager$particle_details_all_csv))
  rank_columns <- grep("^match_rank_[0-9]+_(name|value)$", shared,
                       value = TRUE)
  detail_columns <- intersect(
    c("max_cor_val", "area_um2", "perimeter_um",
      "max_length_um", "min_length_um", "material_class", rank_columns),
    shared
  )
  detail_equal <- isTRUE(all.equal(
    file_backed$particle_details_all_csv[, detail_columns, with = FALSE],
    eager$particle_details_all_csv[, detail_columns, with = FALSE],
    tolerance = tolerance, check.attributes = FALSE
  ))
  file_processed <- file_backed$samples[[1L]]$particles_rds
  eager_processed <- eager$samples[[1L]]$particles_rds
  spectra_equal <- isTRUE(all.equal(
    file_processed$spectra, eager_processed$spectra,
    tolerance = tolerance, check.attributes = FALSE
  ))
  if (!detail_equal || !spectra_equal) {
    stop("File-backed and eager particle results were not equivalent",
         call. = FALSE)
  }
  TRUE
}

run_file_specs_benchmark <- function() {
  repetitions <- .bench_number(
    "OPENSPECY_FILE_SPECS_BENCH_REPS", 3, integer = TRUE
  )
  selection_n <- .bench_number(
    "OPENSPECY_FILE_SPECS_BENCH_SELECTION", 256, integer = TRUE
  )
  use_peak <- .bench_flag("OPENSPECY_FILE_SPECS_BENCH_PEAK")
  keep_cache <- .bench_flag("OPENSPECY_FILE_SPECS_BENCH_KEEP_CACHE")
  bench_root <- tempfile("openspecy-filespec-benchmark-")
  dir.create(bench_root)
  if (!keep_cache) {
    on.exit(unlink(bench_root, recursive = TRUE, force = TRUE), add = TRUE)
  }

  external <- c(
    h5 = Sys.getenv("OPENSPECY_H5_BENCH_FILE", ""),
    envi = Sys.getenv("OPENSPECY_ENVI_BENCH_FILE", "")
  )
  external <- external[nzchar(external)]
  missing <- external[!file.exists(external)]
  if (length(missing)) {
    stop("Benchmark source does not exist: ", paste(missing, collapse = ", "),
         call. = FALSE)
  }

  tiny <- NULL
  if (!length(external) || !"envi" %in% names(external)) {
    tiny <- .bench_make_tiny_envi(bench_root)
  }
  sources <- external
  if (!is.null(tiny)) sources <- c(sources, tiny_envi = tiny$path)

  results <- list()
  descriptors <- list()
  snapshots <- list()
  for (source_name in names(sources)) {
    path <- sources[[source_name]]
    results[[paste0(source_name, "_open")]] <- .bench_repeat(
      paste0(source_name, ":open_index"),
      function() open_specs(path, cache_dir = tempfile(
        paste0(source_name, "-open-"), tmpdir = bench_root
      )),
      repetitions, use_peak
    )
    cache <- file.path(bench_root, paste0(source_name, "-selection-cache"))
    descriptors[[source_name]] <- open_specs(path, cache_dir = cache)
    snapshots[[source_name]] <- .bench_snapshot(descriptors[[source_name]])
    n_spectra <- OpenSpecy:::.filespec_n_spectra(descriptors[[source_name]])
    positions <- unique(as.integer(round(seq(
      1, n_spectra, length.out = min(selection_n, n_spectra)
    ))))
    results[[paste0(source_name, "_preview")]] <- .bench_repeat(
      paste0(source_name, ":one_spectrum_preview"),
      function() decompress_spec(descriptors[[source_name]],
                                 index = positions[[1L]]),
      repetitions, use_peak
    )
    results[[paste0(source_name, "_selection")]] <- .bench_repeat(
      paste0(source_name, ":bounded_selection"),
      function() decompress_spec(descriptors[[source_name]], index = positions),
      repetitions, use_peak
    )
    source_bytes <- sum(descriptors[[source_name]]$source$members$size)
    descriptor_bytes <- as.numeric(object.size(descriptors[[source_name]]))
    selected_payload <- length(positions) *
      length(OpenSpecy:::.filespec_axis(descriptors[[source_name]])) * 8
    for (key in c(paste0(source_name, "_open"),
                  paste0(source_name, "_preview"),
                  paste0(source_name, "_selection"))) {
      results[[key]]$source_bytes <- source_bytes
      results[[key]]$descriptor_bytes <- descriptor_bytes
      results[[key]]$estimated_payload_bytes <- if (grepl("selection", key)) {
        selected_payload
      } else if (grepl("preview", key)) {
        length(OpenSpecy:::.filespec_axis(descriptors[[source_name]])) * 8
      } else 0
      results[[key]]$cache_bytes <- .bench_dir_bytes(cache)
      results[[key]]$source_passes <- if (grepl("selection|preview", key)) 1 else 0
    }
  }

  oracle_path <- Sys.getenv("OPENSPECY_RDS_BENCH_FILE", "")
  eager_oracle <- NULL
  if (nzchar(oracle_path)) {
    if (!file.exists(oracle_path)) {
      stop("OPENSPECY_RDS_BENCH_FILE does not exist", call. = FALSE)
    }
    results$eager_load <- .bench_repeat(
      "eager_oracle:rds_load", function() readRDS(oracle_path),
      repetitions, use_peak
    )
    eager_oracle <- readRDS(oracle_path)
    if (!is_OpenSpecy(eager_oracle)) {
      stop("The eager oracle must be one OpenSpecy object", call. = FALSE)
    }
    results$eager_load$source_bytes <- file.info(oracle_path)$size
    results$eager_load$descriptor_bytes <- as.numeric(object.size(eager_oracle))
    results$eager_load$estimated_payload_bytes <- file.info(oracle_path)$size
    results$eager_load$cache_bytes <- 0
    results$eager_load$source_passes <- 1

    comparison_name <- if ("h5" %in% names(descriptors)) "h5" else
      if ("envi" %in% names(descriptors)) "envi" else ""
    if (nzchar(comparison_name)) {
      region <- Sys.getenv("OPENSPECY_FILE_SPECS_BENCH_REGION", "Region1")
      comparison_view <- .bench_region_view(descriptors[[comparison_name]],
                                            region)
      count <- min(selection_n,
                   OpenSpecy:::.filespec_n_spectra(comparison_view),
                   ncol(eager_oracle$spectra))
      positions <- unique(as.integer(round(seq(
        1, min(OpenSpecy:::.filespec_n_spectra(comparison_view),
               ncol(eager_oracle$spectra)), length.out = count
      ))))
      file_subset <- function() decompress_spec(comparison_view,
                                                index = positions)
      eager_subset <- function() .bench_eager_subset(eager_oracle, positions)
      results$file_oracle_subset <- .bench_repeat(
        paste0(comparison_name, ":oracle_bounded_selection"),
        file_subset, repetitions, use_peak
      )
      results$eager_oracle_subset <- .bench_repeat(
        "eager_oracle:bounded_selection", eager_subset,
        repetitions, use_peak
      )
      observed <- file_subset()
      expected <- eager_subset()
      equivalent <- isTRUE(all.equal(
        observed$wavenumber, expected$wavenumber,
        tolerance = 1e-10, check.attributes = FALSE
      )) && isTRUE(all.equal(
        observed$spectra, expected$spectra,
        tolerance = 1e-10, check.attributes = FALSE
      ))
      if (!equivalent) {
        stop("File-backed bounded selection did not match the eager RDS oracle",
             call. = FALSE)
      }
      payload <- length(positions) * length(observed$wavenumber) * 8
      results$file_oracle_subset$source_bytes <-
        sum(comparison_view$source$members$size)
      results$file_oracle_subset$descriptor_bytes <-
        as.numeric(object.size(comparison_view))
      results$file_oracle_subset$estimated_payload_bytes <- payload
      results$file_oracle_subset$cache_bytes <-
        .bench_dir_bytes(comparison_view$cache$root)
      results$file_oracle_subset$source_passes <- 1
      results$eager_oracle_subset$source_bytes <- 0
      results$eager_oracle_subset$descriptor_bytes <-
        as.numeric(object.size(eager_oracle))
      results$eager_oracle_subset$estimated_payload_bytes <- payload
      results$eager_oracle_subset$cache_bytes <- 0
      results$eager_oracle_subset$source_passes <- 0
    }
  }

  library_path <- Sys.getenv("OPENSPECY_PARTICLE_LIBRARY_RDS", "")
  pipeline_name <- if (nzchar(library_path) && "h5" %in% names(external)) {
    "h5"
  } else if (nzchar(library_path) && "envi" %in% names(external)) {
    "envi"
  } else if (!is.null(tiny)) {
    "tiny_envi"
  } else {
    ""
  }

  if (nzchar(pipeline_name)) {
    library <- if (identical(pipeline_name, "tiny_envi")) {
      tiny$library
    } else {
      if (!file.exists(library_path)) {
        stop("OPENSPECY_PARTICLE_LIBRARY_RDS does not exist", call. = FALSE)
      }
      readRDS(library_path)
    }
    args <- .bench_particle_args(
      library, tiny = identical(pipeline_name, "tiny_envi")
    )
    region <- Sys.getenv("OPENSPECY_FILE_SPECS_BENCH_REGION", "Region1")
    source_path <- sources[[pipeline_name]]
    cold_fun <- function() {
      cache <- tempfile("cold-", tmpdir = bench_root)
      specs <- open_specs(source_path, cache_dir = cache)
      view <- .bench_region_view(specs, region)
      do.call(automate_particle_analysis, c(list(x = view), args))
    }
    results$particle_cold <- .bench_repeat(
      paste0(pipeline_name, ":particle_cold"), cold_fun,
      repetitions, use_peak
    )

    warm_cache <- file.path(bench_root, "particle-warm-cache")
    warm_specs <- open_specs(source_path, cache_dir = warm_cache)
    warm_view <- .bench_region_view(warm_specs, region)
    pipeline <- function(x) {
      do.call(automate_particle_analysis, c(list(x = x), args))
    }
    invisible(pipeline(warm_view))
    results$particle_warm <- .bench_repeat(
      paste0(pipeline_name, ":particle_warm_cache"),
      function() pipeline(warm_view), repetitions, use_peak
    )
    results$particle_cold$source_passes <- 2
    results$particle_warm$source_passes <- 0
    for (key in c("particle_cold", "particle_warm")) {
      results[[key]]$source_bytes <- sum(warm_specs$source$members$size)
      results[[key]]$descriptor_bytes <- as.numeric(object.size(warm_specs))
      results[[key]]$estimated_payload_bytes <- NA_real_
      results[[key]]$cache_bytes <- .bench_dir_bytes(warm_cache)
    }

    eager <- NULL
    if (identical(pipeline_name, "tiny_envi")) {
      eager <- decompress_spec(warm_view, region = region)
    } else if (!is.null(eager_oracle)) {
      eager <- eager_oracle
    }
    if (!is.null(eager)) {
      if (!is_OpenSpecy(eager)) {
        stop("The eager oracle must be one OpenSpecy object", call. = FALSE)
      }
      results$particle_eager <- .bench_repeat(
        "eager_oracle:particle", function() pipeline(eager),
        repetitions, use_peak
      )
      results$particle_eager$source_bytes <- 0
      results$particle_eager$descriptor_bytes <- as.numeric(object.size(eager))
      results$particle_eager$estimated_payload_bytes <- as.numeric(object.size(eager))
      results$particle_eager$cache_bytes <- 0
      results$particle_eager$source_passes <- 0
      .bench_compare_particle_results(pipeline(warm_view), pipeline(eager))
    } else {
      message("Skipping eager particle comparison; set ",
              "OPENSPECY_RDS_BENCH_FILE to the matching regional OpenSpecy RDS.")
    }
  } else {
    message("Skipping particle benchmark; set a reference-library RDS for an ",
            "external source, or omit ENVI to use the tiny ENVI fixture.")
  }

  for (source_name in names(descriptors)) {
    after <- .bench_snapshot(descriptors[[source_name]])
    if (!identical(snapshots[[source_name]], after) ||
        !isTRUE(check_Specs(descriptors[[source_name]]))) {
      stop(source_name, " source changed during the benchmark", call. = FALSE)
    }
  }

  columns <- c("case", "iteration", "elapsed_seconds", "allocated_mb",
               "peak_ram_mb", "source_bytes", "descriptor_bytes",
               "estimated_payload_bytes", "cache_bytes", "source_passes")
  output <- do.call(rbind, lapply(results, function(x) {
    missing <- setdiff(columns, names(x))
    for (name in missing) x[[name]] <- NA
    x[, columns]
  }))
  rownames(output) <- NULL
  print(output)
  cat("\nMedian results:\n")
  medians <- lapply(split(output, output$case), function(rows) {
    metric <- function(x) if (all(is.na(x))) NA_real_ else
      stats::median(x, na.rm = TRUE)
    data.frame(
      case = rows$case[[1L]],
      elapsed_seconds = metric(rows$elapsed_seconds),
      allocated_mb = metric(rows$allocated_mb),
      peak_ram_mb = metric(rows$peak_ram_mb)
    )
  })
  print(do.call(rbind, medians), row.names = FALSE)
  if (!use_peak) {
    message("allocated_mb is the Rprofmem allocation total, not peak RSS. ",
            "Set OPENSPECY_FILE_SPECS_BENCH_PEAK=true and install peakRAM ",
            "to record peak_ram_mb.")
  }
  output_path <- Sys.getenv("OPENSPECY_FILE_SPECS_BENCH_OUTPUT", "")
  if (nzchar(output_path)) {
    if (file.exists(output_path)) {
      stop("Benchmark output already exists: ", output_path, call. = FALSE)
    }
    dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
    stage <- tempfile("filespec-benchmark-", tmpdir = dirname(output_path),
                      fileext = ".csv")
    utils::write.csv(output, stage, row.names = FALSE)
    if (!file.rename(stage, output_path)) {
      unlink(stage, force = TRUE)
      stop("Could not publish benchmark CSV", call. = FALSE)
    }
  }
  if (keep_cache) message("Benchmark cache retained at: ", bench_root)
  invisible(output)
}

if (.bench_flag("OPENSPECY_RUN_FILE_SPECS_BENCHMARK")) {
  run_file_specs_benchmark()
} else {
  message("FileSpecs benchmark skipped; set ",
          "OPENSPECY_RUN_FILE_SPECS_BENCHMARK=true to run it.")
}
