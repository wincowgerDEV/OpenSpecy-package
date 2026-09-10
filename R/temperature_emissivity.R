#' Estimate material temperature and an emissivity diagnostic
#'
#' @description
#' `estimate_temperature()` applies an experimental, spectrally smooth
#' temperature-emissivity separation (TES) model to calibrated FTIR
#' thermal-emission radiance. The primary `OpenSpecy` method evaluates spectra
#' in bounded, BLAS-backed blocks and returns one compact row per input
#' spectrum. It does not create a full emissivity cube.
#'
#' The input must contain calibrated, surface-leaving spectral radiance in
#' `"W m^-2 sr^-1 (cm^-1)^-1"`. This is not suitable for absorbance,
#' transmittance, reflectance, normalized intensities, or detector counts.
#'
#' @param x An `OpenSpecy` or `FileSpecs` object containing calibrated FTIR
#'   surface-leaving spectral radiance.
#' @param downwelling Required background/downwelling radiance. Supply a
#'   numeric scalar, a numeric vector aligned to `x$wavenumber`, or a
#'   one-spectrum calibrated-radiance `OpenSpecy` object on the same axis.
#'   Numeric zero explicitly requests the no-background assumption.
#' @param temperature_range_k A finite, increasing two-value temperature search
#'   interval in kelvin. The endpoints are rejection boundaries, not valid
#'   estimates.
#' @param fit_range_cm1 A finite two-value fitting interval in inverse
#'   centimetres. Select a range for which the opaque, isothermal,
#'   surface-leaving model is valid.
#' @param radiance_uncertainty Optional positive radiance uncertainty, supplied
#'   as a numeric scalar/vector or one-spectrum `OpenSpecy`. It inverse-variance
#'   weights spectral curvature and excludes trial-temperature channels whose
#'   blackbody/downwelling separation is too small for stable inversion.
#' @param emissivity_stat One scalar emissivity reduction per spectrum:
#'   `"planck_weighted"` (the default band-effective directional value),
#'   `"mean"`, `"median"`, or `"max"`.
#' @param block_size Optional positive whole number of in-memory spectra per
#'   compute block. `NULL` derives a bounded value from the number of fitting
#'   bands. It changes memory/time trade-offs, not results.
#' @param ... Additional arguments passed to methods.
#'
#' @return `estimate_temperature()` returns a source-aligned `data.table` with
#'   estimated material temperature, one selected emissivity value, roughness,
#'   physical- and valid-band fractions, fitting-band provenance, and a status.
#'   Only `status == "ok"` rows contain estimates. `calculate_emissivity()`
#'   returns an `OpenSpecy` object with unclipped emissivity spectra.
#'
#' @details
#' The surface model is
#' `L = epsilon * B(T) + (1 - epsilon) * L_down`, for an opaque isothermal
#' target. Because temperature-emissivity separation is underdetermined, the
#' selected temperature is conditional on a smooth-emissivity prior. A unique
#' interior roughness minimum is required. Boundary, flat, multiple,
#' unresolved, near-singular, and insufficient-band cases remain aligned but
#' return `NA` estimates and a diagnostic status.
#'
#' `planck_weighted` integrates the retrieved spectral emissivity over the
#' fitting band using Planck radiance and trapezoidal wavenumber weights. It is
#' a band-effective directional value; it is not necessarily the total
#' hemispherical emissivity listed in material tables. Emissivity also depends
#' on wavelength, temperature, viewing geometry, surface finish, oxidation,
#' particle thickness, and sub-pixel mixing.
#'
#' No values are clipped to `[0, 1]`. The physical fraction reports how much of
#' the retrieved curve lies in that interval, making calibration/model failures
#' visible. Use direct radiance or established signal/noise contrast as the
#' baseline for particle detection until TES metrics are validated on held-out
#' measurements.
#'
#' @references
#' National Bureau of Standards. *Radiometric temperature measurements: II.
#' Applications* (Technical Note 910-8).
#' \url{https://nvlpubs.nist.gov/nistpubs/Legacy/TN/nbstechnicalnote910-8.pdf}
#'
#' Borel CC (1997). Iterative retrieval of surface emissivity and temperature
#' for a hyperspectral sensor. \url{https://digital.library.unt.edu/ark:/67531/metadc696880/}
#'
#' Wilber AC, Kratz DP, Gupta SK (1999). Surface emissivity maps for use in
#' satellite retrievals of longwave radiation. NASA/TP-1999-209362.
#' \url{https://ntrs.nasa.gov/citations/19990100634}
#'
#' Wu Z, Ren H, Zhang T, Qin Q, Dong J, Ye X (2017). A modified method to
#' prevent false minimums occurring in iterative spectrally smooth temperature
#' emissivity separation. \doi{10.1109/IGARSS.2017.8128417}.
#'
#' @seealso [calculate_emissivity()], [sig_noise()], [def_features()]
#' @export
estimate_temperature <- function(x, ...) {
  UseMethod("estimate_temperature")
}

#' @rdname estimate_temperature
#' @export
estimate_temperature.default <- function(x, ...) {
  stop("object 'x' needs to be of class 'OpenSpecy' or 'FileSpecs'",
       call. = FALSE)
}

#' @rdname estimate_temperature
#' @export
estimate_temperature.OpenSpecy <- function(
    x, downwelling, temperature_range_k, fit_range_cm1,
    radiance_uncertainty = NULL,
    emissivity_stat = c("planck_weighted", "mean", "median", "max"),
    block_size = NULL, ...) {
  .thermal_reject_dots(...)
  x <- as_OpenSpecy(x, compute_file_id = FALSE)
  .thermal_validate_radiance_source(x)
  .thermal_validate_spectra(x$spectra)
  emissivity_stat <- match.arg(emissivity_stat)
  temperature_range_k <- .thermal_temperature_range(temperature_range_k)
  fit_range_cm1 <- .thermal_fit_range(fit_range_cm1)

  .thermal_validate_axis(x$wavenumber)
  source_axis <- as.numeric(x$wavenumber)
  ord <- order(source_axis)
  axis <- source_axis[ord]
  down <- .thermal_common_radiance(downwelling, source_axis, "downwelling")
  down <- down[ord]
  uncertainty <- .thermal_common_radiance(
    radiance_uncertainty, source_axis, "radiance_uncertainty",
    allow_null = TRUE, positive = TRUE
  )
  if (!is.null(uncertainty)) uncertainty <- uncertainty[ord]

  bands <- which(axis >= fit_range_cm1[[1L]] &
                   axis <= fit_range_cm1[[2L]])
  base <- .thermal_openspecy_index(x)
  if (length(bands) < 5L) {
    fit <- .thermal_empty_fit(ncol(x$spectra), "insufficient_bands")
  } else {
    block_size <- .thermal_block_size(block_size, length(bands))
    fit <- .thermal_empty_fit(ncol(x$spectra), "not_evaluated")
    source_bands <- ord[bands]
    chunks <- split(seq_len(ncol(x$spectra)),
                    ceiling(seq_len(ncol(x$spectra)) / block_size))
    for (chunk_index in seq_along(chunks)) {
      cols <- chunks[[chunk_index]]
      # Select columns before rows so no whole-map fit-band copy is created.
      block <- x$spectra[source_bands, cols, drop = FALSE]
      chunk_fit <- .estimate_temperature_matrix(
        wavenumber = axis[bands], spectra = block,
        downwelling = down[bands],
        temperature_range_k = temperature_range_k,
        radiance_uncertainty = if (is.null(uncertainty)) NULL else {
          uncertainty[bands]
        },
        emissivity_stat = emissivity_stat, block_size = length(cols)
      )
      for (nm in names(fit)) fit[[nm]][cols] <- chunk_fit[[nm]]
    }
  }
  .thermal_result_table(
    base, fit, emissivity_stat, fit_range_cm1, temperature_range_k,
    downwelling = down, radiance_uncertainty = uncertainty
  )
}

#' @rdname estimate_temperature
#' @export
estimate_temperature.FileSpecs <- function(
    x, downwelling, temperature_range_k, fit_range_cm1,
    radiance_uncertainty = NULL,
    emissivity_stat = c("planck_weighted", "mean", "median", "max"),
    block_size = NULL, ...) {
  .thermal_reject_dots(...)
  .filespec_validate_object(x)
  .filespec_validate_source(x, strong = FALSE)
  .thermal_validate_radiance_source(x)
  emissivity_stat <- match.arg(emissivity_stat)
  temperature_range_k <- .thermal_temperature_range(temperature_range_k)
  fit_range_cm1 <- .thermal_fit_range(fit_range_cm1)

  source_axis <- .filespec_axis(x)
  .thermal_validate_axis(source_axis)
  ord <- order(source_axis)
  axis <- source_axis[ord]
  down <- .thermal_common_radiance(downwelling, source_axis, "downwelling")
  down <- down[ord]
  uncertainty <- .thermal_common_radiance(
    radiance_uncertainty, source_axis, "radiance_uncertainty",
    allow_null = TRUE, positive = TRUE
  )
  if (!is.null(uncertainty)) uncertainty <- uncertainty[ord]
  sorted_bands <- which(axis >= fit_range_cm1[[1L]] &
                          axis <= fit_range_cm1[[2L]])
  source_bands <- ord[sorted_bands]
  index <- .filespec_index(x)
  base <- .thermal_filespec_index(index)
  block_size <- .thermal_block_size(block_size, max(1L, length(sorted_bands)))

  cache_key <- digest::digest(list(
    schema = "temperature-emissivity-1", source = x$source$id,
    view = x$view, temperature_range_k = temperature_range_k,
    fit_range_cm1 = fit_range_cm1, downwelling = down,
    radiance_uncertainty = uncertainty, emissivity_stat = emissivity_stat,
    provenance = list(
      intensity_unit = .thermal_metadata_values(x, "intensity_unit"),
      spectra_type = .thermal_metadata_values(x, "spectra_type"),
      radiance_level = .thermal_metadata_values(x, "radiance_level"),
      radiometric_calibration =
        .thermal_metadata_values(x, "radiometric_calibration")
    )
  ), algo = "sha256")
  cache_file <- .filespec_cache_path(
    x, "temperature-emissivity", paste0(cache_key, ".rds")
  )
  if (file.exists(cache_file)) {
    cached <- tryCatch(readRDS(cache_file), error = function(e) NULL)
    if (data.table::is.data.table(cached) && nrow(cached) == nrow(base)) {
      return(cached)
    }
  }

  if (length(sorted_bands) < 5L) {
    fit <- .thermal_empty_fit(nrow(index), "insufficient_bands")
  } else {
    fit <- .thermal_empty_fit(nrow(index), "not_evaluated")
    chunks <- split(seq_len(nrow(index)),
                    ceiling(seq_len(nrow(index)) / block_size))
    for (rows in chunks) {
      values <- .filespec_read_values(x, index = rows, bands = source_bands)
      chunk_fit <- .estimate_temperature_matrix(
        wavenumber = values$wavenumber, spectra = values$spectra,
        downwelling = down[sorted_bands],
        temperature_range_k = temperature_range_k,
        radiance_uncertainty = if (is.null(uncertainty)) NULL else {
          uncertainty[sorted_bands]
        },
        emissivity_stat = emissivity_stat,
        block_size = min(block_size, length(rows))
      )
      for (nm in names(fit)) fit[[nm]][rows] <- chunk_fit[[nm]]
    }
  }
  out <- .thermal_result_table(
    base, fit, emissivity_stat, fit_range_cm1, temperature_range_k,
    downwelling = down, radiance_uncertainty = uncertainty
  )
  .filespec_atomic_save_rds(out, cache_file, compress = FALSE)
  out
}

#' Calculate spectral emissivity at supplied material temperatures
#'
#' @description
#' Materialize spectral emissivity for a selected or particle-collapsed
#' calibrated-radiance `OpenSpecy` object. Estimate temperature after particle
#' collapse because TES is nonlinear. Values outside `[0, 1]` are retained as
#' calibration and model diagnostics.
#'
#' @param x A selected or collapsed calibrated-radiance `OpenSpecy` object.
#' @param temperature_k A positive material temperature in kelvin, supplied as
#'   one scalar or one value per spectrum.
#' @param downwelling Required scalar, bandwise numeric, or one-spectrum
#'   `OpenSpecy` downwelling radiance.
#' @param block_size Optional positive whole-number compute block size. `NULL`
#'   selects a bounded value automatically.
#' @param ... Additional arguments passed to methods.
#'
#' @return An aligned `OpenSpecy` object whose spectra and per-spectrum units
#'   are emissivity. The supplied temperatures and radiative-transfer model
#'   are appended to metadata, and transformation provenance is recorded.
#'
#' @seealso [estimate_temperature()]
#' @export
calculate_emissivity <- function(x, ...) {
  UseMethod("calculate_emissivity")
}

#' @rdname calculate_emissivity
#' @export
calculate_emissivity.default <- function(x, ...) {
  stop("object 'x' needs to be of class 'OpenSpecy'", call. = FALSE)
}

#' @rdname calculate_emissivity
#' @export
calculate_emissivity.OpenSpecy <- function(
    x, temperature_k, downwelling, block_size = NULL, ...) {
  .thermal_reject_dots(...)
  x <- as_OpenSpecy(x, compute_file_id = FALSE)
  .thermal_validate_radiance_source(x)
  .thermal_validate_spectra(x$spectra)
  n <- ncol(x$spectra)
  if (!is.numeric(temperature_k) || !length(temperature_k) ||
      anyNA(temperature_k) || any(!is.finite(temperature_k)) ||
      any(temperature_k <= 0) || !length(temperature_k) %in% c(1L, n)) {
    stop("'temperature_k' must contain one positive finite kelvin value or ",
         "one value per spectrum", call. = FALSE)
  }
  temperature_k <- rep(as.numeric(temperature_k), length.out = n)
  .thermal_validate_axis(x$wavenumber)
  source_axis <- as.numeric(x$wavenumber)
  down <- .thermal_common_radiance(downwelling, source_axis, "downwelling")
  block_size <- .thermal_block_size(block_size, length(source_axis))
  emissivity <- matrix(
    NA_real_, nrow = nrow(x$spectra), ncol = n,
    dimnames = dimnames(x$spectra)
  )
  chunks <- split(seq_len(n), ceiling(seq_len(n) / block_size))
  for (cols in chunks) {
    blackbody <- .planck_radiance_wavenumber(
      source_axis, temperature_k[cols]
    )
    if (length(cols) == 1L) blackbody <- matrix(blackbody, ncol = 1L)
    denominator <- sweep(blackbody, 1L, down, "-")
    source_block <- x$spectra[, cols, drop = FALSE]
    valid <- .thermal_valid_denominator(blackbody, down, NULL) &
      is.finite(source_block)
    numerator <- sweep(source_block, 1L, down, "-")
    block <- numerator / denominator
    block[!valid] <- NA_real_
    emissivity[, cols] <- block
  }
  x$spectra <- emissivity
  x$metadata <- data.table::copy(x$metadata)
  x$metadata[, material_temperature_k := temperature_k]
  x$metadata[, temperature_source := "supplied"]
  if ("intensity_unit" %in% names(x$metadata)) {
    x$metadata[, intensity_unit := "emissivity"]
  }
  x$metadata[, intensity_units := "emissivity"]
  x$metadata[, emissivity_model := "opaque_isothermal_surface_radiance"]
  attr(x, "intensity_unit") <- "emissivity"
  if (!is.null(attr(x, "intensity_units", exact = TRUE))) {
    attr(x, "intensity_units") <- "emissivity"
  }
  attr(x, "derivative_order") <- "0"
  attr(x, "baseline") <- "raw"
  attr(x, "spectra_type") <- "ftir"
  .append_specs_transformation(x, list(
    method = "calculate_emissivity",
    model = "opaque_isothermal_surface_radiance",
    temperature_k_sha256 = .thermal_digest(temperature_k),
    downwelling_sha256 = .thermal_digest(down),
    lossy = TRUE
  ))
}

.thermal_radiance_unit <- "W m^-2 sr^-1 (cm^-1)^-1"

# Stable Planck spectral radiance per inverse centimetre. For a vector of
# temperatures, rows are wavenumbers and columns are temperatures.
.planck_radiance_wavenumber <- function(wavenumber, temperature_k) {
  if (!is.numeric(wavenumber) || !length(wavenumber) || anyNA(wavenumber) ||
      any(!is.finite(wavenumber)) || any(wavenumber <= 0)) {
    stop("'wavenumber' must contain positive finite values in cm^-1",
         call. = FALSE)
  }
  if (!is.numeric(temperature_k) || !length(temperature_k) ||
      anyNA(temperature_k) || any(!is.finite(temperature_k)) ||
      any(temperature_k <= 0)) {
    stop("'temperature_k' must contain positive finite kelvin values",
         call. = FALSE)
  }
  h <- 6.62607015e-34
  speed_of_light <- 299792458
  boltzmann <- 1.380649e-23
  wavenumber_m <- as.numeric(wavenumber) * 100
  exponent <- tcrossprod(
    h * speed_of_light * wavenumber_m / boltzmann,
    1 / as.numeric(temperature_k)
  )
  inverse_expm1 <- matrix(0, nrow = nrow(exponent), ncol = ncol(exponent))
  safe <- exponent < 700
  inverse_expm1[safe] <- 1 / expm1(exponent[safe])
  prefactor <- 2 * h * speed_of_light^2 * wavenumber_m^3 * 100
  out <- sweep(inverse_expm1, 1L, prefactor, "*")
  if (length(temperature_k) == 1L) as.numeric(out[, 1L]) else out
}

.thermal_temperature_range <- function(x) {
  if (!is.numeric(x) || length(x) != 2L || anyNA(x) ||
      any(!is.finite(x)) || any(x <= 0) || x[[1L]] >= x[[2L]]) {
    stop("'temperature_range_k' must be two increasing positive finite ",
         "kelvin values", call. = FALSE)
  }
  as.numeric(x)
}

.thermal_fit_range <- function(x) {
  if (!is.numeric(x) || length(x) != 2L || anyNA(x) ||
      any(!is.finite(x)) || any(x <= 0) || x[[1L]] == x[[2L]]) {
    stop("'fit_range_cm1' must contain two distinct positive finite values",
         call. = FALSE)
  }
  sort(as.numeric(x))
}

.thermal_reject_dots <- function(...) {
  dots <- list(...)
  if (length(dots)) {
    stop("unused argument(s): ", paste(names(dots), collapse = ", "),
         call. = FALSE)
  }
  invisible(NULL)
}

.thermal_block_size <- function(block_size, nband,
                                target_bytes = 27 * 1024^2) {
  if (is.null(block_size)) {
    # Roughly eight band-by-spectrum temporaries can be live with missing data.
    return(max(1L, min(8192L, as.integer(floor(
      target_bytes / (max(1, nband) * 8 * 8)
    )))))
  }
  if (!is.numeric(block_size) || length(block_size) != 1L ||
      is.na(block_size) || !is.finite(block_size) || block_size < 1L ||
      block_size > .Machine$integer.max || block_size != floor(block_size)) {
    stop("'block_size' must be NULL or one positive whole number",
         call. = FALSE)
  }
  as.integer(block_size)
}

.thermal_metadata_values <- function(x, name) {
  object_value <- attr(x, name, exact = TRUE)
  metadata_value <- NULL
  if (inherits(x, "OpenSpecy") && name %in% names(x$metadata)) {
    metadata_value <- x$metadata[[name]]
  }
  values <- unname(unlist(c(list(object_value), list(metadata_value)),
                          recursive = TRUE, use.names = FALSE))
  if (length(values)) values else NULL
}

.thermal_validate_radiance_unit <- function(x, name = "x") {
  values <- c(.thermal_metadata_values(x, "intensity_unit"),
              .thermal_metadata_values(x, "intensity_units"))
  if (is.null(values) || anyNA(values) ||
      any(!nzchar(trimws(as.character(values))))) {
    stop("'", name, "' must declare calibrated spectral radiance units as ",
         "'", .thermal_radiance_unit, "'", call. = FALSE)
  }
  values <- unique(trimws(as.character(values)))
  if (length(values) != 1L || !identical(values, .thermal_radiance_unit)) {
    stop("'", name, "' must declare calibrated spectral radiance units as ",
         "'", .thermal_radiance_unit, "'", call. = FALSE)
  }
  invisible(TRUE)
}

.thermal_validate_radiance_source <- function(x) {
  .thermal_validate_radiance_unit(x)
  type <- c(.thermal_metadata_values(x, "spectra_type"),
            .thermal_metadata_values(x, "spectrum_type"))
  if (is.null(type) || anyNA(type) ||
      any(!nzchar(trimws(as.character(type))))) {
    stop("'x' must be explicitly labelled as FTIR spectra", call. = FALSE)
  }
  type <- unique(tolower(trimws(as.character(type))))
  if (length(type) != 1L || !identical(type, "ftir")) {
    stop("'x' must be explicitly labelled as FTIR spectra", call. = FALSE)
  }

  level <- .thermal_metadata_values(x, "radiance_level")
  calibration <- .thermal_metadata_values(x, "radiometric_calibration")
  incomplete <- is.null(level) || anyNA(level) ||
    any(!nzchar(trimws(as.character(level)))) || is.null(calibration) ||
    anyNA(calibration) || any(!nzchar(trimws(as.character(calibration))))
  if (incomplete) {
    stop("'x' needs surface-leaving radiance and nonempty radiometric ",
         "calibration provenance", call. = FALSE)
  }
  level <- unique(gsub("-", "_", tolower(trimws(as.character(level)))))
  if (length(level) != 1L || !identical(level, "surface_leaving")) {
    stop("'x' needs surface-leaving radiance and nonempty radiometric ",
         "calibration provenance", call. = FALSE)
  }

  derivative <- .thermal_metadata_values(x, "derivative_order")
  derivative <- unique(as.character(derivative[!is.na(derivative) &
                                                nzchar(derivative)]))
  if (length(derivative) && any(!derivative %in% c("0", "0.0"))) {
    stop("derivative-processed radiance cannot be used for TES",
         call. = FALSE)
  }
  baseline <- .thermal_metadata_values(x, "baseline")
  baseline <- unique(tolower(as.character(
    baseline[!is.na(baseline) & nzchar(baseline)]
  )))
  if (length(baseline) && any(!baseline %in% "raw")) {
    stop("baseline-processed radiance cannot be used for TES",
         call. = FALSE)
  }
  procedure <- .thermal_metadata_values(x, "data_processing_procedure")
  if (length(procedure)) {
    procedure <- tolower(as.character(procedure[!is.na(procedure)]))
    negative_baseline <- grepl(
      "(^|[^a-z])(no|not|without)[ -]+baseline([ -]+correction)?([^a-z]|$)",
      procedure
    )
    procedure[negative_baseline] <- gsub(
      "(^|[^a-z])(no|not|without)[ -]+baseline([ -]+correction)?([^a-z]|$)",
      " ", procedure[negative_baseline]
    )
    if (any(grepl("normaliz|min[ -]?max|derivat|baseline", procedure))) {
      stop("normalized, derivative, or baseline-processed radiance cannot ",
           "be used for TES", call. = FALSE)
    }
  }
  transformations <- attr(x, "transformations", exact = TRUE)
  if (length(transformations)) {
    methods <- vapply(transformations, function(record) {
      if (is.list(record) && length(record$method)) {
        as.character(record$method[[1L]])
      } else {
        ""
      }
    }, character(1L))
    if (any(grepl("make_rel|normaliz|derivat|baseline", tolower(methods)))) {
      stop("normalized, derivative, or baseline-processed radiance cannot ",
           "be used for TES", call. = FALSE)
    }
  }
  invisible(TRUE)
}

.thermal_validate_axis <- function(wavenumber) {
  if (!is.numeric(wavenumber) || is.complex(wavenumber) ||
      !length(wavenumber) || anyNA(wavenumber) ||
      any(!is.finite(wavenumber)) || any(wavenumber <= 0) ||
      anyDuplicated(wavenumber)) {
    stop("'x' wavenumbers must be unique positive finite numeric values in ",
         "cm^-1", call. = FALSE)
  }
  invisible(TRUE)
}

.thermal_validate_spectra <- function(spectra) {
  if (!is.matrix(spectra) || !is.numeric(spectra) || is.complex(spectra)) {
    stop("'x' spectra must be stored as a real numeric matrix", call. = FALSE)
  }
  invisible(TRUE)
}

.thermal_digest <- function(value) {
  digest::digest(as.numeric(value), algo = "sha256")
}

.thermal_common_radiance <- function(value, source_axis, name,
                                     allow_null = FALSE, positive = FALSE) {
  if (is.null(value)) {
    if (isTRUE(allow_null)) return(NULL)
    stop("'", name, "' is required", call. = FALSE)
  }
  if (inherits(value, "OpenSpecy")) {
    value <- as_OpenSpecy(value, compute_file_id = FALSE)
    .thermal_validate_radiance_unit(value, name)
    .thermal_validate_axis(value$wavenumber)
    .thermal_validate_spectra(value$spectra)
    if (ncol(value$spectra) != 1L) {
      stop("'", name, "' OpenSpecy input must contain one spectrum",
           call. = FALSE)
    }
    value_axis <- as.numeric(value$wavenumber)
    if (length(value$wavenumber) != length(source_axis) ||
        !isTRUE(all.equal(sort(value_axis), sort(source_axis),
                          tolerance = 1e-10, check.attributes = FALSE))) {
      stop("'", name, "' OpenSpecy axis must contain the source bands",
           call. = FALSE)
    }
    value_order <- order(value_axis)
    source_order <- order(source_axis)
    sorted_values <- as.numeric(value$spectra[value_order, 1L])
    out <- numeric(length(source_axis))
    out[source_order] <- sorted_values
  } else {
    if (!is.numeric(value) || !length(value) ||
        !length(value) %in% c(1L, length(source_axis))) {
      stop("'", name, "' must be a numeric scalar, a vector with the ",
           "source-axis length, or a one-spectrum OpenSpecy", call. = FALSE)
    }
    out <- rep(as.numeric(value), length.out = length(source_axis))
  }
  if (anyNA(out) || any(!is.finite(out))) {
    stop("'", name, "' must contain only finite values", call. = FALSE)
  }
  if (isTRUE(positive) && any(out <= 0)) {
    stop("'", name, "' must contain positive values", call. = FALSE)
  }
  out
}

.thermal_curvature_coefficients <- function(x) {
  if (length(x) < 3L || any(diff(x) <= 0)) {
    stop("fitting wavenumbers must be strictly increasing", call. = FALSE)
  }
  left <- diff(x)[seq_len(length(x) - 2L)]
  right <- diff(x)[seq.int(2L, length(x) - 1L)]
  span <- left + right
  list(
    a0 = 2 / (left * span),
    a1 = -2 / (left * right),
    a2 = 2 / (right * span)
  )
}

.thermal_valid_denominator <- function(blackbody, downwelling, uncertainty) {
  denominator <- sweep(blackbody, 1L, downwelling, "-")
  scale <- pmax(abs(blackbody), abs(downwelling))
  tolerance <- sqrt(.Machine$double.eps) * scale + .Machine$double.xmin
  if (!is.null(uncertainty)) tolerance <- pmax(tolerance, uncertainty)
  is.finite(denominator) & abs(denominator) > tolerance
}

.thermal_prepare_scores <- function(wavenumber, temperatures, downwelling,
                                    radiance_uncertainty) {
  blackbody <- .planck_radiance_wavenumber(wavenumber, temperatures)
  if (length(temperatures) == 1L) blackbody <- matrix(blackbody, ncol = 1L)
  denominator <- sweep(blackbody, 1L, downwelling, "-")
  valid <- .thermal_valid_denominator(
    blackbody, downwelling, radiance_uncertainty
  )
  q <- matrix(0, nrow = nrow(blackbody), ncol = ncol(blackbody))
  q[valid] <- 1 / denominator[valid]
  coefficients <- .thermal_curvature_coefficients(wavenumber)
  p <- length(wavenumber)
  nt <- length(temperatures)
  q2 <- t(q^2)
  valid_band_count <- colSums(valid)
  row_weight <- matrix(0, nrow = p - 2L, ncol = nt)

  for (j in seq_len(nt)) {
    row_valid <- valid[seq_len(p - 2L), j] &
      valid[seq.int(2L, p - 1L), j] & valid[seq.int(3L, p), j]
    if (!any(row_valid)) next
    if (is.null(radiance_uncertainty)) {
      weights <- rep(1, p - 2L)
    } else {
      u0 <- coefficients$a0 * radiance_uncertainty[seq_len(p - 2L)] *
        q[seq_len(p - 2L), j]
      u1 <- coefficients$a1 * radiance_uncertainty[seq.int(2L, p - 1L)] *
        q[seq.int(2L, p - 1L), j]
      u2 <- coefficients$a2 * radiance_uncertainty[seq.int(3L, p)] *
        q[seq.int(3L, p), j]
      variance <- u0^2 + u1^2 + u2^2
      weights <- numeric(p - 2L)
      weights[row_valid] <- 1 / pmax(variance[row_valid],
                                     .Machine$double.xmin)
      weights[row_valid] <- weights[row_valid] /
        mean(weights[row_valid])
    }
    weights[!row_valid] <- 0
    row_weight[, j] <- weights
  }
  list(
    temperatures = temperatures, blackbody = blackbody, q = q,
    valid = valid, q2 = q2, valid_band_count = valid_band_count,
    row_weight = row_weight,
    row_weight_sum = colSums(row_weight), coefficients = coefficients,
    downwelling = downwelling, uncertainty = radiance_uncertainty
  )
}

# Direct curvature evaluation avoids the subtractive cancellation of an
# expanded banded quadratic near a genuinely smooth solution. The positive
# scale term remains one BLAS matrix product per shared temperature grid.
# Missing values are zero-filled only inside those products and excluded again
# through explicit band/curvature masks, so common detector gaps stay on the
# vectorized path without being treated as measured zeroes.
.thermal_score_matrix_direct <- function(prepared, numerator) {
  score <- matrix(NA_real_, nrow = length(prepared$temperatures),
                  ncol = ncol(numerator))
  p <- nrow(numerator)
  observed <- is.finite(numerator)
  all_observed <- all(observed)
  common_observed <- !all_observed && all(
    rowSums(observed) %in% c(0L, ncol(numerator))
  )
  if (all_observed) {
    values <- numerator
    observed <- NULL
    squares <- values^2
  } else if (common_observed) {
    observed_bands <- observed[, 1L]
    values <- numerator
    squares <- values^2
    squares[!observed_bands, ] <- 0
    observed <- NULL
  } else {
    values <- numerator
    values[!observed] <- 0
    squares <- values^2
  }
  scale <- prepared$q2 %*% squares
  if (all_observed) {
    valid_band_count <- prepared$valid_band_count
    observed_rows <- NULL
  } else if (common_observed) {
    valid_band_count <- colSums(prepared$valid & observed_bands)
    observed_rows <- observed_bands[seq_len(p - 2L)] &
      observed_bands[seq.int(2L, p - 1L)] &
      observed_bands[seq.int(3L, p)]
  } else {
    observed_numeric <- matrix(as.numeric(observed), nrow = p)
    valid_band_count <- crossprod(prepared$valid * 1, observed_numeric)
    observed_rows <- observed[seq_len(p - 2L), , drop = FALSE] &
      observed[seq.int(2L, p - 1L), , drop = FALSE] &
      observed[seq.int(3L, p), , drop = FALSE]
    observed_rows_numeric <- matrix(as.numeric(observed_rows), nrow = p - 2L)
  }
  scale <- scale / pmax(valid_band_count, 1L)
  first <- values[seq_len(p - 2L), , drop = FALSE]
  middle <- values[seq.int(2L, p - 1L), , drop = FALSE]
  last <- values[seq.int(3L, p), , drop = FALSE]
  effective_rows <- matrix(
    NA_real_, nrow = length(prepared$temperatures), ncol = ncol(numerator)
  )
  for (j in seq_along(prepared$temperatures)) {
    weights <- prepared$row_weight[, j]
    if (all_observed || common_observed) {
      if (all_observed) {
        weight_sum_value <- prepared$row_weight_sum[[j]]
        weight_square_sum_value <- sum(weights^2)
      } else {
        weight_sum_value <- sum(weights[observed_rows])
        weight_square_sum_value <- sum(weights[observed_rows]^2)
      }
      weight_sum <- rep(weight_sum_value, ncol(numerator))
      weight_square_sum <- rep(weight_square_sum_value, ncol(numerator))
    } else {
      weight_sum <- as.numeric(crossprod(weights, observed_rows_numeric))
      weight_square_sum <- as.numeric(crossprod(
        weights^2, observed_rows_numeric
      ))
    }
    effective_rows[j, ] <- weight_sum^2 / pmax(
      weight_square_sum, .Machine$double.xmin
    )
    c0 <- prepared$coefficients$a0 * prepared$q[seq_len(p - 2L), j]
    c1 <- prepared$coefficients$a1 * prepared$q[seq.int(2L, p - 1L), j]
    c2 <- prepared$coefficients$a2 * prepared$q[seq.int(3L, p), j]
    curvature <- c0 * first + c1 * middle + c2 * last
    if (common_observed) {
      curvature[!observed_rows, ] <- 0
    } else if (!all_observed) {
      curvature[!observed_rows] <- 0
    }
    roughness <- colSums(
      curvature^2 * weights, na.rm = TRUE
    ) / pmax(weight_sum, .Machine$double.xmin)
    valid_count_j <- if (all_observed || common_observed) {
      rep(valid_band_count[[j]], ncol(numerator))
    } else {
      valid_band_count[j, ]
    }
    eligible <- valid_count_j >= 5L & effective_rows[j, ] >= 3L
    score[j, eligible] <- roughness[eligible] /
      pmax(scale[j, eligible], .Machine$double.eps)
  }
  list(score = score, effective_rows = effective_rows)
}

.thermal_select_local_minimum <- function(score, effective_rows) {
  if (is.null(dim(score))) score <- matrix(score, ncol = 1L)
  nt <- nrow(score)
  n <- ncol(score)
  if (is.null(dim(effective_rows))) {
    if (length(effective_rows) == 1L) {
      effective_rows <- matrix(effective_rows, nrow = nt, ncol = n)
    } else if (length(effective_rows) == nt) {
      effective_rows <- matrix(rep(effective_rows, n), nrow = nt)
    } else {
      stop("Internal effective-row dimensions do not match scores.",
           call. = FALSE)
    }
  } else if (nrow(effective_rows) != nt ||
             !ncol(effective_rows) %in% c(1L, n)) {
    stop("Internal effective-row dimensions do not match scores.",
         call. = FALSE)
  } else if (ncol(effective_rows) == 1L && n > 1L) {
    effective_rows <- matrix(rep(effective_rows, n), nrow = nt)
  }
  out_index <- rep(NA_integer_, n)
  out_status <- rep("near_singular", n)
  finite <- is.finite(score)
  finite_count <- colSums(finite)
  enough <- finite_count >= 3L
  if (!any(enough)) return(list(index = out_index, status = out_status))

  finite_score <- score
  finite_score[!finite] <- NA_real_
  minimum_values <- matrixStats::colMins(finite_score, na.rm = TRUE)
  maximum_values <- matrixStats::colMaxs(finite_score, na.rm = TRUE)
  value_scale <- matrixStats::colMaxs(abs(finite_score), na.rm = TRUE)
  numerical_by_column <- pmax(
    .Machine$double.xmin,
    100 * .Machine$double.eps * value_scale
  )
  flat <- enough &
    (maximum_values - minimum_values <= pmax(
      .Machine$double.xmin, value_scale * 1e-10
    ))
  out_status[flat] <- "flat_objective"

  score_for_minimum <- score
  score_for_minimum[!finite] <- Inf
  global_index <- max.col(t(-score_for_minimum), ties.method = "first")
  numerical <- matrix(
    rep(numerical_by_column, each = nt), nrow = nt, ncol = n
  )
  global_minimum <- matrix(
    rep(minimum_values, each = nt), nrow = nt, ncol = n
  )
  global_tie <- colSums(
    finite & abs(score - global_minimum) <= numerical,
    na.rm = TRUE
  ) > 1L
  boundary <- enough & !flat & !global_tie &
    global_index %in% c(1L, nt)
  out_status[boundary] <- "boundary_minimum"

  shoulder <- score
  shoulder[!finite] <- -Inf
  left_shoulder <- shoulder
  right_shoulder <- shoulder
  for (index in seq.int(2L, nt)) {
    left_shoulder[index, ] <- pmax(
      left_shoulder[index - 1L, ], shoulder[index, ]
    )
  }
  for (index in seq.int(nt - 1L, 1L)) {
    right_shoulder[index, ] <- pmax(
      right_shoulder[index + 1L, ], shoulder[index, ]
    )
  }
  prominence <- pmin(left_shoulder, right_shoulder) - score
  sampling_resolution <- pmax(score, 0) * sqrt(2 / effective_rows)
  resolved <- matrix(FALSE, nrow = nt, ncol = n)
  interior <- seq.int(2L, nt - 1L)
  previous <- score[interior - 1L, , drop = FALSE]
  current <- score[interior, , drop = FALSE]
  following <- score[interior + 1L, , drop = FALSE]
  neighbour_drop <- pmin(previous, following) - current
  local <- is.finite(previous) & is.finite(current) & is.finite(following) &
    current < previous & current <= following
  resolved[interior, ] <- local &
    is.finite(effective_rows[interior, , drop = FALSE]) &
    effective_rows[interior, , drop = FALSE] >= 3 &
    is.finite(prominence[interior, , drop = FALSE]) &
    prominence[interior, , drop = FALSE] > pmax(
      numerical[interior, , drop = FALSE],
      sampling_resolution[interior, , drop = FALSE]
    ) & is.finite(neighbour_drop) &
    neighbour_drop > numerical[interior, , drop = FALSE]

  resolved_count <- colSums(resolved)
  global_resolved <- resolved[cbind(global_index, seq_len(n))]
  unresolved <- enough & !flat & !global_tie & !boundary & !global_resolved
  multiple <- enough & !flat & (global_tie |
    (!boundary & global_resolved & resolved_count > 1L))
  candidate <- enough & !flat & !global_tie & !boundary & global_resolved &
    resolved_count == 1L
  out_status[unresolved] <- "unresolved_minimum"
  out_status[multiple] <- "multiple_minima"
  out_status[candidate] <- "candidate"
  out_index[candidate] <- global_index[candidate]
  list(index = out_index, status = out_status)
}

.thermal_fit_complete <- function(numerator, wavenumber, downwelling,
                                  temperature_range_k, radiance_uncertainty) {
  coarse_temperatures <- seq(temperature_range_k[[1L]],
                             temperature_range_k[[2L]], length.out = 17L)
  coarse <- .thermal_prepare_scores(
    wavenumber, coarse_temperatures, downwelling, radiance_uncertainty
  )
  coarse_result <- .thermal_score_matrix_direct(coarse, numerator)
  score <- coarse_result$score
  selected <- .thermal_select_local_minimum(
    score, coarse_result$effective_rows
  )
  temperatures <- rep(NA_real_, ncol(numerator))
  status <- selected$status
  candidates <- which(status == "candidate")
  if (!length(candidates)) return(list(temperature = temperatures,
                                       status = status))

  groups <- split(candidates, selected$index[candidates])
  for (group_name in names(groups)) {
    coarse_index <- as.integer(group_name)
    cols <- groups[[group_name]]
    fine_temperatures <- seq(coarse_temperatures[coarse_index - 1L],
                             coarse_temperatures[coarse_index + 1L],
                             length.out = 9L)
    fine <- .thermal_prepare_scores(
      wavenumber, fine_temperatures, downwelling, radiance_uncertainty
    )
    fine_result <- .thermal_score_matrix_direct(
      fine, numerator[, cols, drop = FALSE]
    )
    fine_score <- fine_result$score
    fine_selected <- .thermal_select_local_minimum(
      fine_score, fine_result$effective_rows
    )
    unresolved <- fine_selected$status != "candidate"
    status[cols[unresolved]] <- ifelse(
      fine_selected$status[unresolved] == "multiple_minima",
      "multiple_minima", "unresolved_minimum"
    )
    resolved <- which(!unresolved)
    if (!length(resolved)) next
    for (k in resolved) {
      idx <- fine_selected$index[[k]]
      y <- fine_score[c(idx - 1L, idx, idx + 1L), k]
      denominator <- y[[1L]] - 2 * y[[2L]] + y[[3L]]
      delta <- if (is.finite(denominator) && denominator > 0) {
        0.5 * (y[[1L]] - y[[3L]]) / denominator
      } else {
        0
      }
      delta <- max(-1, min(1, delta))
      step <- fine_temperatures[[2L]] - fine_temperatures[[1L]]
      temperatures[cols[[k]]] <- fine_temperatures[[idx]] + delta * step
      status[cols[[k]]] <- "ok"
    }
  }
  list(temperature = temperatures, status = status)
}

.thermal_trapezoid_weights <- function(x) {
  gaps <- diff(x)
  c(gaps[[1L]] / 2,
    (gaps[-length(gaps)] + gaps[-1L]) / 2,
    gaps[[length(gaps)]] / 2)
}

.thermal_summarize_block <- function(wavenumber, spectra, downwelling,
                                     temperatures, radiance_uncertainty,
                                     emissivity_stat) {
  n <- ncol(spectra)
  out <- list(
    emissivity = rep(NA_real_, n), roughness = rep(NA_real_, n),
    physical_fraction = rep(NA_real_, n), valid_fraction = rep(NA_real_, n),
    valid = rep(FALSE, n)
  )
  good <- which(is.finite(temperatures))
  if (!length(good)) return(out)
  blackbody <- .planck_radiance_wavenumber(
    wavenumber, temperatures[good]
  )
  if (length(good) == 1L) blackbody <- matrix(blackbody, ncol = 1L)
  denominator <- sweep(blackbody, 1L, downwelling, "-")
  valid <- .thermal_valid_denominator(
    blackbody, downwelling, radiance_uncertainty
  ) & is.finite(spectra[, good, drop = FALSE])
  numerator <- sweep(spectra[, good, drop = FALSE], 1L, downwelling, "-")
  epsilon <- numerator / denominator
  epsilon[!valid] <- NA_real_
  valid_count <- colSums(valid)
  enough <- valid_count >= 5L
  out$valid_fraction[good] <- valid_count / nrow(spectra)
  out$physical_fraction[good] <- colSums(
    epsilon >= 0 & epsilon <= 1, na.rm = TRUE
  ) / pmax(valid_count, 1L)

  summary_value <- switch(
    emissivity_stat,
    mean = colMeans(epsilon, na.rm = TRUE),
    median = matrixStats::colMedians(epsilon, na.rm = TRUE),
    max = matrixStats::colMaxs(epsilon, na.rm = TRUE),
    planck_weighted = {
      quadrature <- .thermal_trapezoid_weights(wavenumber)
      weight <- blackbody * quadrature
      weight[!valid] <- NA_real_
      colSums(epsilon * weight, na.rm = TRUE) /
        colSums(weight, na.rm = TRUE)
    }
  )
  coefficients <- .thermal_curvature_coefficients(wavenumber)
  curvature <- coefficients$a0 * epsilon[seq_len(nrow(epsilon) - 2L), ,
                                          drop = FALSE] +
    coefficients$a1 * epsilon[seq.int(2L, nrow(epsilon) - 1L), ,
                              drop = FALSE] +
    coefficients$a2 * epsilon[seq.int(3L, nrow(epsilon)), , drop = FALSE]
  roughness <- colMeans(curvature^2, na.rm = TRUE)
  roughness[!is.finite(roughness)] <- NA_real_
  summary_value[!is.finite(summary_value)] <- NA_real_
  out$emissivity[good] <- summary_value
  out$roughness[good] <- roughness
  out$valid[good] <- enough & is.finite(summary_value)
  out
}

.estimate_temperature_matrix <- function(
    wavenumber, spectra, downwelling, temperature_range_k,
    radiance_uncertainty = NULL,
    emissivity_stat = c("planck_weighted", "mean", "median", "max"),
    block_size = NULL) {
  emissivity_stat <- match.arg(emissivity_stat)
  wavenumber <- as.numeric(wavenumber)
  ord <- order(wavenumber)
  if (!identical(ord, seq_along(wavenumber))) {
    wavenumber <- wavenumber[ord]
    spectra <- spectra[ord, , drop = FALSE]
    downwelling <- as.numeric(downwelling)[ord]
    if (!is.null(radiance_uncertainty)) {
      radiance_uncertainty <- as.numeric(radiance_uncertainty)[ord]
    }
  } else {
    downwelling <- as.numeric(downwelling)
    if (!is.null(radiance_uncertainty)) {
      radiance_uncertainty <- as.numeric(radiance_uncertainty)
    }
  }
  block_size <- .thermal_block_size(block_size, length(wavenumber))
  n <- ncol(spectra)
  result <- .thermal_empty_fit(n, "not_evaluated")
  chunks <- split(seq_len(n), ceiling(seq_len(n) / block_size))
  for (cols in chunks) {
    block <- spectra[, cols, drop = FALSE]
    numerator <- sweep(block, 1L, downwelling, "-")
    fit_temperature <- rep(NA_real_, length(cols))
    fit_status <- rep("near_singular", length(cols))
    enough_bands <- colSums(is.finite(numerator)) >= 5L
    fit_status[!enough_bands] <- "insufficient_bands"
    if (any(enough_bands)) {
      fit <- .thermal_fit_complete(
        numerator[, enough_bands, drop = FALSE], wavenumber, downwelling,
        temperature_range_k, radiance_uncertainty
      )
      fit_temperature[enough_bands] <- fit$temperature
      fit_status[enough_bands] <- fit$status
    }
    summary <- .thermal_summarize_block(
      wavenumber, block, downwelling, fit_temperature,
      radiance_uncertainty, emissivity_stat
    )
    invalid_summary <- fit_status == "ok" & !summary$valid
    fit_status[invalid_summary] <- "near_singular"
    fit_temperature[fit_status != "ok"] <- NA_real_
    summary$emissivity[fit_status != "ok"] <- NA_real_
    summary$roughness[fit_status != "ok"] <- NA_real_
    summary$physical_fraction[fit_status != "ok"] <- NA_real_
    result$temperature[cols] <- fit_temperature
    result$emissivity[cols] <- summary$emissivity
    result$roughness[cols] <- summary$roughness
    result$physical_fraction[cols] <- summary$physical_fraction
    result$valid_fraction[cols] <- summary$valid_fraction
    result$status[cols] <- fit_status
  }
  result
}

.thermal_empty_fit <- function(n, status) {
  list(
    temperature = rep(NA_real_, n), emissivity = rep(NA_real_, n),
    roughness = rep(NA_real_, n), physical_fraction = rep(NA_real_, n),
    valid_fraction = rep(NA_real_, n), status = rep(status, n)
  )
}

.thermal_openspecy_index <- function(x) {
  metadata <- data.table::as.data.table(x$metadata)
  ids <- colnames(x$spectra)
  if (is.null(ids)) ids <- paste0("V", seq_len(ncol(x$spectra)))
  if ("col_id" %in% names(metadata)) ids <- as.character(metadata$col_id)
  out <- data.table::data.table(
    spectrum_index = seq_len(ncol(x$spectra)), spectrum_id = ids
  )
  coordinate_names <- intersect(c("region", "row", "col", "x", "y"),
                                names(metadata))
  if (length(coordinate_names)) {
    out <- cbind(out, metadata[, coordinate_names, with = FALSE])
  }
  out
}

.thermal_filespec_index <- function(index) {
  out <- data.table::data.table(
    spectrum_index = seq_len(nrow(index)),
    source_index = as.integer(index$index),
    spectrum_id = as.character(index$col_id)
  )
  coordinate_names <- intersect(c("region", "row", "col", "x", "y"),
                                names(index))
  if (length(coordinate_names)) {
    out <- cbind(out, index[, coordinate_names, with = FALSE])
  }
  out
}

.thermal_result_table <- function(base, fit, emissivity_stat, fit_range_cm1,
                                  temperature_range_k, downwelling,
                                  radiance_uncertainty) {
  out <- data.table::copy(base)
  out[, estimated_material_temperature_k := fit$temperature]
  out[, emissivity_value := fit$emissivity]
  out[, emissivity_statistic := emissivity_stat]
  out[, emissivity_roughness := fit$roughness]
  out[, emissivity_physical_fraction := fit$physical_fraction]
  out[, valid_band_fraction := fit$valid_fraction]
  out[, fit_min_cm1 := fit_range_cm1[[1L]]]
  out[, fit_max_cm1 := fit_range_cm1[[2L]]]
  out[, status := fit$status]
  attr(out, "temperature_emissivity_settings") <- list(
    schema = "temperature-emissivity-1",
    radiance_unit = .thermal_radiance_unit,
    temperature_range_k = temperature_range_k,
    fit_range_cm1 = fit_range_cm1,
    emissivity_statistic = emissivity_stat,
    downwelling_sha256 = .thermal_digest(downwelling),
    radiance_uncertainty_sha256 = if (is.null(radiance_uncertainty)) {
      NULL
    } else {
      .thermal_digest(radiance_uncertainty)
    },
    model = "opaque_isothermal_surface_radiance"
  )
  out
}
