# Internal memory preflight helpers used by the bundled Shiny application.
# These stay deliberately dependency-free because the same package code is
# loaded by desktop R and WebAssembly builds.

.app_memory_probe_cache <- new.env(parent = emptyenv())
.app_memory_probe_cache$attempted <- FALSE
.app_memory_probe_cache$result <- NULL

.app_reset_memory_probe_cache <- function() {
  .app_memory_probe_cache$attempted <- FALSE
  .app_memory_probe_cache$result <- NULL
  invisible(NULL)
}

.app_memory_unknown <- function(source, reason, cached = FALSE) {
  list(
    bytes = NA_real_,
    known = FALSE,
    source = as.character(source)[1L],
    reason = as.character(reason)[1L],
    cached = isTRUE(cached)
  )
}

.app_memory_known <- function(bytes, source, cached = FALSE) {
  list(
    bytes = as.numeric(bytes),
    known = TRUE,
    source = as.character(source)[1L],
    reason = NULL,
    cached = isTRUE(cached)
  )
}

.app_valid_memory_bytes <- function(x) {
  is.numeric(x) && length(x) == 1L && !is.na(x) && is.finite(x) && x >= 0
}

.app_normalize_memory_probe <- function(value, default_source) {
  source <- default_source
  reason <- "The memory probe did not return a finite byte count."

  if(is.list(value)) {
    if(!is.null(value$source) && length(value$source)) {
      source <- as.character(value$source)[1L]
    }
    if(!is.null(value$reason) && length(value$reason)) {
      reason <- as.character(value$reason)[1L]
    }
    value <- value$bytes
  }

  if(.app_valid_memory_bytes(value)) {
    return(.app_memory_known(value, source))
  }

  .app_memory_unknown(source, reason)
}

.app_windows_available_memory <- function() {
  candidates <- Sys.which(c("powershell.exe", "powershell", "pwsh.exe",
                            "pwsh"))
  shell <- unname(candidates[nzchar(candidates)])[1L]
  if(is.na(shell) || !nzchar(shell)) {
    stop("PowerShell was not found.", call. = FALSE)
  }

  command <- paste0(
    "Add-Type -AssemblyName Microsoft.VisualBasic; ",
    "[Console]::Write(([Microsoft.VisualBasic.Devices.ComputerInfo]",
    "::new()).AvailablePhysicalMemory)"
  )
  output <- suppressWarnings(system2(
    shell,
    c("-NoProfile", "-NonInteractive", "-Command", shQuote(command)),
    stdout = TRUE,
    stderr = TRUE
  ))
  values <- suppressWarnings(as.numeric(trimws(output)))
  values <- values[is.finite(values)]
  if(!length(values)) {
    stop("PowerShell did not report available physical memory.", call. = FALSE)
  }

  list(
    bytes = values[[1L]],
    source = "PowerShell ComputerInfo:AvailablePhysicalMemory"
  )
}

.app_linux_available_memory <- function() {
  path <- "/proc/meminfo"
  if(!file.exists(path)) {
    stop("/proc/meminfo is unavailable.", call. = FALSE)
  }

  lines <- readLines(path, warn = FALSE)
  available <- grep("^MemAvailable:[[:space:]]+", lines, value = TRUE)
  if(length(available) != 1L) {
    stop("MemAvailable is missing from /proc/meminfo.", call. = FALSE)
  }
  kib <- suppressWarnings(as.numeric(sub(
    "^MemAvailable:[[:space:]]+([0-9]+)[[:space:]]+kB.*$",
    "\\1",
    available
  )))
  if(!.app_valid_memory_bytes(kib)) {
    stop("MemAvailable could not be parsed.", call. = FALSE)
  }

  list(bytes = kib * 1024, source = "/proc/meminfo:MemAvailable")
}

.app_macos_available_memory <- function() {
  output <- suppressWarnings(system2(
    "vm_stat", stdout = TRUE, stderr = TRUE
  ))
  if(!length(output)) {
    stop("vm_stat did not return memory statistics.", call. = FALSE)
  }

  page_line <- grep("page size of [0-9]+ bytes", output, value = TRUE)
  if(!length(page_line)) {
    stop("The vm_stat page size could not be parsed.", call. = FALSE)
  }
  page_size <- suppressWarnings(as.numeric(sub(
    ".*page size of ([0-9]+) bytes.*", "\\1", page_line[[1L]]
  )))

  page_names <- c("Pages free", "Pages inactive", "Pages speculative")
  pages <- vapply(page_names, function(name) {
    line <- grep(paste0("^", name, ":[[:space:]]+"), output, value = TRUE)
    if(!length(line)) return(0)
    suppressWarnings(as.numeric(sub(
      "^.*:[[:space:]]+([0-9]+)\\..*$", "\\1", line[[1L]]
    )))
  }, numeric(1L))

  if(!.app_valid_memory_bytes(page_size) || any(!is.finite(pages))) {
    stop("vm_stat available pages could not be parsed.", call. = FALSE)
  }
  list(
    bytes = page_size * sum(pages),
    source = "vm_stat:free+inactive+speculative"
  )
}

.app_platform_memory_probe <- function() {
  platform <- paste(
    R.version$platform,
    if(is.null(R.version$arch)) "" else R.version$arch
  )
  if(grepl("emscripten|wasm", platform, ignore.case = TRUE)) {
    return(.app_memory_unknown(
      "WebAssembly runtime",
      "The browser runtime does not expose dependable host RAM availability."
    ))
  }

  sysname <- tryCatch(Sys.info()[["sysname"]], error = function(e) NA_character_)
  if(identical(sysname, "Windows")) return(.app_windows_available_memory())
  if(identical(sysname, "Linux")) return(.app_linux_available_memory())
  if(identical(sysname, "Darwin")) return(.app_macos_available_memory())

  .app_memory_unknown(
    paste0("unsupported platform: ", ifelse(is.na(sysname), "unknown", sysname)),
    "No dependency-free available-memory probe is defined for this platform."
  )
}

.app_available_memory <- function(available_bytes = NULL, probe = NULL) {
  if(!is.null(available_bytes)) {
    if(.app_valid_memory_bytes(available_bytes)) {
      return(.app_memory_known(available_bytes, "argument"))
    }
    return(.app_memory_unknown(
      "argument", "'available_bytes' must be one finite, non-negative number."
    ))
  }

  option_bytes <- getOption("OpenSpecy.available_memory_bytes", NULL)
  if(!is.null(option_bytes)) {
    if(.app_valid_memory_bytes(option_bytes)) {
      return(.app_memory_known(
        option_bytes, "option:OpenSpecy.available_memory_bytes"
      ))
    }
    return(.app_memory_unknown(
      "option:OpenSpecy.available_memory_bytes",
      "The configured memory override is not a finite, non-negative number."
    ))
  }

  if(isTRUE(.app_memory_probe_cache$attempted)) {
    result <- .app_memory_probe_cache$result
    result$cached <- TRUE
    return(result)
  }

  option_probe <- getOption("OpenSpecy.memory_probe", NULL)
  if(is.null(probe) && is.function(option_probe)) {
    probe <- option_probe
    source <- "option:OpenSpecy.memory_probe"
  } else if(is.function(probe)) {
    source <- "injected memory probe"
  } else {
    sysname <- tryCatch(
      Sys.info()[["sysname"]], error = function(e) NA_character_
    )
    source <- paste0(
      "platform probe:", ifelse(is.na(sysname), "unknown", sysname)
    )
    probe <- .app_platform_memory_probe
  }

  result <- tryCatch(
    .app_normalize_memory_probe(probe(), source),
    error = function(e) .app_memory_unknown(source, conditionMessage(e))
  )
  # A failed live probe is abandoned after its first pass, as the app then
  # proceeds with an explicit unknown-RAM status. Successful live readings are
  # deliberately not cached: free memory changes as datasets, libraries, and
  # processing intermediates are created or released.
  if(!isTRUE(result$known)) {
    .app_memory_probe_cache$attempted <- TRUE
    .app_memory_probe_cache$result <- result
  }
  result
}

.app_memory_count <- function(x, name, default = 0L, positive = FALSE) {
  if(is.null(x) && !is.null(default)) x <- default
  valid <- is.numeric(x) && length(x) == 1L && !is.na(x) && is.finite(x) &&
    x <= .Machine$integer.max && x == floor(x) &&
    if(positive) x >= 1 else x >= 0
  if(!valid) {
    qualifier <- if(positive) "positive" else "non-negative"
    stop("'", name, "' must be a ", qualifier, " integer.", call. = FALSE)
  }
  as.integer(x)
}

.app_format_bytes <- function(bytes) {
  if(length(bytes) != 1L || is.na(bytes)) return("unknown")
  if(is.infinite(bytes)) return("more than addressable memory")

  units <- c("B", "KiB", "MiB", "GiB", "TiB", "PiB")
  unit <- min(length(units), floor(log(max(bytes, 1), base = 1024)) + 1L)
  value <- bytes / 1024^(unit - 1L)
  paste0(format(round(value, 1L), nsmall = 1L, trim = TRUE), " ", units[[unit]])
}

.app_memory_remedies <- function(safe, peak_phase, additional_peak_bytes,
                                 memory, reserve_bytes, top_n, block_size,
                                 pca_components, clusters,
                                 estimated_loaded_bytes) {
  loaded_context <- paste0(
    "Loaded spectra and reference-library estimate: ",
    .app_format_bytes(estimated_loaded_bytes), "."
  )
  if(is.na(safe)) {
    reason <- if(is.null(memory$reason) || !nzchar(memory$reason)) {
      "no reason was reported"
    } else {
      memory$reason
    }
    return(c(
      loaded_context,
      paste0(
        "Available RAM could not be measured (", memory$source, ": ",
        reason, "). ",
        "Safety is unknown, so this estimate is not being treated as safe."
      ),
      paste0(
        "The app can continue under the 10 GB upload ceiling, but split or ",
        "crop the dataset if processing exhausts memory."
      )
    ))
  }

  if(isTRUE(safe)) {
    return(c(
      loaded_context,
      paste0(
        "Estimated additional peak memory is ",
        .app_format_bytes(additional_peak_bytes),
        ", within the measured RAM after a ",
        .app_format_bytes(reserve_bytes), " reserve."
      )
    ))
  }

  remedies <- c(
    loaded_context,
    paste0(
      "This workflow may need ", .app_format_bytes(additional_peak_bytes),
      " of additional RAM; only ",
      .app_format_bytes(memory$bytes - reserve_bytes),
      " remains after the safety reserve."
    ),
    "Crop or split the hyperspectral map, or close other applications and retry."
  )

  if(identical(peak_phase, "blockwise_matching")) {
    top_n_remedy <- if(top_n > 1L) {
      paste0("Lower Top N from ", top_n, " to retain fewer rows, ")
    } else {
      "Top N is already at its minimum; "
    }
    remedies <- c(remedies, paste0(
      "Matching already uses blocks of ", block_size, " spectra. ",
      top_n_remedy,
      "choose a smaller library, or use a smaller block."
    ))
  }
  if(identical(peak_phase, "pca_clustering")) {
    settings <- character()
    if(pca_components > 0L) {
      settings <- c(settings, paste0("PCA components from ", pca_components))
    }
    if(clusters > 0L) {
      settings <- c(settings, paste0("clusters from ", clusters))
    }
    setting_remedy <- if(length(settings)) {
      paste0("Lower ", paste(settings, collapse = " or "), ", or ")
    } else {
      "Avoid PCA work, or"
    }
    remedies <- c(remedies, paste0(
      setting_remedy,
      " use connected threshold regions without spectral clustering."
    ))
  }
  if(identical(peak_phase, "preprocessing")) {
    remedies <- c(
      remedies,
      "Reduce optional preprocessing steps or preprocess smaller map sections."
    )
  }
  remedies
}

.app_memory_preflight <- function(object, library_size = 0L, top_n = 10L,
                                  block_size = 100L, pca_components = 0L,
                                  clusters = 0L, available_bytes = NULL,
                                  reserve_fraction = 0.25, probe = NULL) {
  if(!is.list(object) || is.null(object$spectra) ||
     length(dim(object$spectra)) != 2L) {
    stop("'object' must contain a two-dimensional 'spectra' element.",
         call. = FALSE)
  }

  wave_count <- nrow(object$spectra)
  spectrum_count <- ncol(object$spectra)
  library_size <- .app_memory_count(library_size, "library_size")
  top_n <- .app_memory_count(top_n, "top_n", positive = TRUE)
  pca_components <- .app_memory_count(
    pca_components, "pca_components"
  )
  clusters <- .app_memory_count(clusters, "clusters")

  candidate_block <- suppressWarnings(tryCatch(
    as.numeric(block_size), error = function(e) NA_real_
  ))
  if(length(candidate_block) != 1L || is.na(candidate_block) ||
     !is.finite(candidate_block) || candidate_block < 1 ||
     candidate_block > .Machine$integer.max ||
     candidate_block != floor(candidate_block)) {
    candidate_block <- 100L
  }
  block_size <- as.integer(candidate_block)

  if(!is.numeric(reserve_fraction) || length(reserve_fraction) != 1L ||
     is.na(reserve_fraction) || !is.finite(reserve_fraction) ||
     reserve_fraction < 0 || reserve_fraction >= 1) {
    stop("'reserve_fraction' must be at least zero and less than one.",
         call. = FALSE)
  }

  resident_bytes <- as.numeric(utils::object.size(object))
  spectra_bytes <- 8 * as.double(wave_count) * as.double(spectrum_count)
  library_bytes <- 8 * as.double(wave_count) * as.double(library_size)
  estimated_loaded_bytes <- resident_bytes + library_bytes
  effective_block <- min(as.double(block_size), as.double(spectrum_count))
  effective_top_n <- min(as.double(top_n), as.double(library_size))

  block_score_bytes <- 8 * as.double(library_size) * effective_block
  compact_top_n_bytes <- 32 * as.double(spectrum_count) * effective_top_n
  block_top_n_bytes <- 32 * effective_block * effective_top_n
  query_block_bytes <- 8 * as.double(wave_count) * effective_block
  avoided_full_matrix_bytes <- 8 * as.double(library_size) *
    as.double(spectrum_count)

  effective_components <- min(
    as.double(pca_components),
    as.double(wave_count),
    as.double(spectrum_count)
  )
  if(clusters > 0L && effective_components == 0 &&
     wave_count > 0L && spectrum_count > 0L) {
    effective_components <- min(10, wave_count, spectrum_count)
  }
  effective_clusters <- min(as.double(clusters), as.double(spectrum_count))

  preprocessing_workspace <- 2 * spectra_bytes
  matching_workspace <- if(library_size > 0L) {
    # Conservative simultaneous allocation model for the app path:
    # a processed and potentially conformed query copy, three library-sized
    # normalization/scale workspaces, a raw/normalized/scaled query block,
    # its score/top-N block, and the retained compact result.
    2 * spectra_bytes + 3 * library_bytes + 3 * query_block_bytes +
      block_score_bytes + block_top_n_bytes + compact_top_n_bytes
  } else {
    0
  }
  clustering_workspace <- if(effective_components > 0) {
    3 * spectra_bytes +
      8 * spectrum_count * effective_components +
      8 * wave_count * effective_components +
      8 * spectrum_count * effective_clusters +
      8 * effective_components * effective_clusters +
      4 * spectrum_count
  } else {
    0
  }

  phase_workspace_bytes <- c(
    preprocessing = preprocessing_workspace,
    blockwise_matching = matching_workspace,
    pca_clustering = clustering_workspace
  )
  phase_peak_bytes <- estimated_loaded_bytes + phase_workspace_bytes
  peak_index <- which.max(phase_peak_bytes)[[1L]]
  peak_phase <- names(phase_peak_bytes)[[peak_index]]
  peak_bytes <- unname(phase_peak_bytes[[peak_index]])
  additional_peak_bytes <- unname(phase_workspace_bytes[[peak_index]])

  memory <- .app_available_memory(
    available_bytes = available_bytes,
    probe = probe
  )
  if(isTRUE(memory$known)) {
    reserve_bytes <- memory$bytes * reserve_fraction
    usable_available_bytes <- memory$bytes - reserve_bytes
    safe <- is.finite(additional_peak_bytes) &&
      additional_peak_bytes <= usable_available_bytes
  } else {
    reserve_bytes <- NA_real_
    usable_available_bytes <- NA_real_
    safe <- NA
  }

  remedies <- .app_memory_remedies(
    safe = safe,
    peak_phase = peak_phase,
    additional_peak_bytes = additional_peak_bytes,
    memory = memory,
    reserve_bytes = reserve_bytes,
    top_n = top_n,
    block_size = block_size,
    pca_components = pca_components,
    clusters = clusters,
    estimated_loaded_bytes = estimated_loaded_bytes
  )

  structure(list(
    safe = safe,
    status = if(is.na(safe)) "unknown" else if(safe) "safe" else "unsafe",
    available_bytes = memory$bytes,
    available_source = memory$source,
    available_reason = memory$reason,
    probe_cached = memory$cached,
    reserve_fraction = reserve_fraction,
    reserve_bytes = reserve_bytes,
    usable_available_bytes = usable_available_bytes,
    resident_bytes = resident_bytes,
    library_resident_bytes = library_bytes,
    estimated_loaded_bytes = estimated_loaded_bytes,
    spectra_bytes = spectra_bytes,
    library_bytes = library_bytes,
    phase_workspace_bytes = phase_workspace_bytes,
    phase_peak_bytes = phase_peak_bytes,
    peak_phase = peak_phase,
    peak_bytes = peak_bytes,
    additional_peak_bytes = additional_peak_bytes,
    block_score_bytes = block_score_bytes,
    block_top_n_bytes = block_top_n_bytes,
    query_block_bytes = query_block_bytes,
    compact_top_n_bytes = compact_top_n_bytes,
    avoided_full_matrix_bytes = avoided_full_matrix_bytes,
    block_size = block_size,
    effective_block_size = as.integer(effective_block),
    top_n = top_n,
    effective_top_n = as.integer(effective_top_n),
    pca_components = pca_components,
    effective_pca_components = as.integer(effective_components),
    clusters = clusters,
    effective_clusters = as.integer(effective_clusters),
    remedies = remedies,
    message = paste(remedies, collapse = " ")
  ), class = "OpenSpecy_memory_preflight")
}
