.new_spatial_calibration <- function(x_origin, y_origin, x_step, y_step,
                                     unit = "pixel", source = NULL) {
  values <- suppressWarnings(as.numeric(c(x_origin, y_origin, x_step, y_step)))
  if (length(values) != 4L || any(!is.finite(values)) ||
      values[[3L]] == 0 || values[[4L]] == 0) return(NULL)
  list(
    x_origin = values[[1L]], y_origin = values[[2L]],
    x_step = values[[3L]], y_step = values[[4L]],
    unit = as.character(unit %||% "pixel")[[1L]], source = source,
    x_direction = if (values[[3L]] > 0) "increasing" else "decreasing",
    y_direction = if (values[[4L]] > 0) "increasing" else "decreasing"
  )
}

.envi_spatial_calibration <- function(header, warn = TRUE) {
  map_info <- header[["map info"]]
  if (!is.null(map_info) && length(map_info)) {
    fields <- trimws(strsplit(as.character(map_info)[[1L]], ",")[[1L]])
    numeric_fields <- suppressWarnings(as.numeric(fields[2:7]))
    if (length(fields) >= 7L && all(is.finite(numeric_fields))) {
      ref_x <- numeric_fields[[1L]]
      ref_y <- numeric_fields[[2L]]
      map_x <- numeric_fields[[3L]]
      map_y <- numeric_fields[[4L]]
      step_x <- numeric_fields[[5L]]
      step_y <- -abs(numeric_fields[[6L]])
      unit_field <- fields[grepl("^units\\s*=", fields, ignore.case = TRUE)]
      unit <- if (length(unit_field)) {
        trimws(sub("^[^=]*=", "", unit_field[[1L]]))
      } else {
        "map unit"
      }
      calibration <- .new_spatial_calibration(
        map_x + (1 - ref_x) * step_x,
        map_y + (1 - ref_y) * step_y,
        step_x, step_y, unit = unit, source = "map info"
      )
      if (!is.null(calibration)) return(calibration)
    }
    if (isTRUE(warn)) {
      warning("ENVI 'map info' does not contain a complete finite origin and ",
              "pixel size; using pixel coordinates", call. = FALSE)
    }
    return(NULL)
  }

  description <- as.character(header[["description"]] %||% "")
  pixel_size <- as.character(header[["pixel size"]] %||% "")
  has_thermo <- nzchar(description) && nzchar(pixel_size) &&
    grepl("(^|[,;[:space:]])X\\s*=", description, ignore.case = TRUE) &&
    grepl("(^|[,;[:space:]])Y\\s*=", description, ignore.case = TRUE)
  if (!has_thermo) return(NULL)

  extract_last <- function(label) {
    hits <- gregexpr(paste0("(?:^|[,;[:space:]])", label,
                             "\\s*=\\s*[-+0-9.eE]+"), description,
                      perl = TRUE, ignore.case = TRUE)
    values <- regmatches(description, hits)[[1L]]
    if (!length(values) || identical(values, character())) return(NA_real_)
    suppressWarnings(as.numeric(sub(".*=\\s*", "", utils::tail(values, 1L))))
  }
  steps <- suppressWarnings(as.numeric(strsplit(pixel_size, "[,;[:space:]]+")[[1L]]))
  steps <- steps[is.finite(steps)]
  if (length(steps) >= 2L) {
    calibration <- .new_spatial_calibration(
      extract_last("X"), extract_last("Y"), steps[[1L]] * 1e6,
      steps[[2L]] * 1e6, unit = "um", source = "description/pixel size"
    )
    if (!is.null(calibration)) return(calibration)
  }
  if (isTRUE(warn)) {
    warning("ENVI Thermo coordinate metadata is incomplete; using pixel ",
            "coordinates", call. = FALSE)
  }
  NULL
}

.h5_spatial_calibrations <- function(region_extents) {
  extents <- Filter(Negate(is.null), region_extents)
  calibrations <- lapply(extents, function(extent) {
    x_step <- if (length(extent$stage_x) > 1L) diff(extent$stage_x)[[1L]] else 1
    y_step <- if (length(extent$stage_y) > 1L) diff(extent$stage_y)[[1L]] else 1
    calibration <- .new_spatial_calibration(
      extent$stage_x[[1L]], extent$stage_y[[1L]], x_step, y_step,
      unit = extent$stage_units %||% "nm", source = extent$source
    )
    if (!is.null(calibration)) calibration$region <- extent$region
    calibration
  })
  names(calibrations) <- vapply(extents, function(extent) {
    as.character(extent$region %||% "")[[1L]]
  }, character(1L))
  calibrations
}

.set_spatial_calibration <- function(x, calibration) {
  if (!is.null(calibration) && length(calibration)) {
    attr(x, "spatial_calibration") <- calibration
  }
  x
}
