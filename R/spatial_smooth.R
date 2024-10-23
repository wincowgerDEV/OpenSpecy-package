#' @title Spatial Smoothing of OpenSpecy Objects
#'
#' @description
#' Applies spatial smoothing to an `OpenSpecy` object using a Gaussian filter.
#'
#' @param x an `OpenSpecy` object.
#' @param sigma a numeric vector specifying the standard deviations for the
#' Gaussian kernel in the x and y dimensions, respectively.
#' @param \dots further arguments passed to or from other methods.
#'
#' @details
#' This function performs spatial smoothing on the spectral data in an `OpenSpecy` object.
#' It assumes that the spatial coordinates are provided in the `metadata` element of the object,
#' specifically in the `x` and `y` columns, and that there is a `col_id` column in `metadata` that
#' matches the column names in the `spectra` data.table.
#'
#' @return
#' An `OpenSpecy` object with smoothed spectra.
#'
#' @author
#' Win Cowger
#'
#' @seealso
#' \code{\link{as_OpenSpecy}()}, \code{\link[mmand]{gaussianSmooth}()}
#'
#' @importFrom data.table data.table melt dcast setkey setnames
#' @importFrom mmand gaussianSmooth
#' @export
spatial_smooth <- function(x, sigma = c(1, 1, 1), ...) {
    # Check that x is an OpenSpecy object
    if (!inherits(x, "OpenSpecy")) {
        stop("x must be an OpenSpecy object.", call. = FALSE)
    }
    
    # Check that metadata contains 'x', 'y', and 'col_id'
    if (is.null(x$metadata) || !all(c("x", "y", "col_id") %in% names(x$metadata))) {
        stop("The OpenSpecy object must have 'x', 'y', and 'col_id' columns in its metadata.", call. = FALSE)
    }
    
    #Avoid data.table notes
    . <- col_id <- y <- wavenumber <- x_idx <- y_idx <- z_idx <- NULL
    
    # Extract wavenumbers and spectra
    wavenumbers <- x$wavenumber
    spectra <- x$spectra  # data.table with spectra columns
    
    # Extract coordinates and ensure they are numeric
    coords <- x$metadata[, .(col_id, x, y)]
    #coords[, `:=`(x = as.numeric(x), y = as.numeric(y))]
    
    # Ensure that 'col_id's in metadata match column names in 'spectra'
    if (!all(coords$col_id %in% names(spectra))) {
        stop("Not all 'col_id's in metadata match column names in spectra.", call. = FALSE)
    }
    
    # Reshape spectra data.table to long format
    dt_long <- data.table::melt(
        spectra,
        measure.vars = names(spectra),
        variable.name = "col_id",
        value.name = "intensity"
    )
    
    # Add wavenumbers and coordinates to dt_long
    dt_long[, wavenumber := rep(wavenumbers, times = ncol(spectra))]
    # Merge coordinates into dt_long using 'col_id'
    dt_long <- merge(dt_long, coords, by = "col_id", all.x = TRUE)
    
    # Check for any missing coordinates
    if (any(is.na(dt_long$x) | is.na(dt_long$y))) {
        stop("Missing spatial coordinates for some spectra.", call. = FALSE)
    }
    
    # Get unique x, y, and wavenumber values
    x_vals <- sort(unique(dt_long$x))
    y_vals <- sort(unique(dt_long$y))
    z_vals <- wavenumbers  # wavenumbers are already sorted
    
    nx <- length(x_vals)
    ny <- length(y_vals)
    nz <- length(z_vals)
    
    # Map x, y, and wavenumber to indices
    dt_long[, x_idx := match(x, x_vals)]
    dt_long[, y_idx := match(y, y_vals)]
    dt_long[, z_idx := match(wavenumber, z_vals)]
    
    # Initialize empty array
    arr <- array(NA_real_, dim = c(nx, ny, nz))
    
    # Fill the array with intensity values
    idx <- cbind(dt_long$x_idx, dt_long$y_idx, dt_long$z_idx)
    arr[idx] <- dt_long$intensity
    
    # Apply Gaussian smoothing (sigma for x and y; 0 for wavenumber dimension)
    arr_smoothed <- mmand::gaussianSmooth(arr, sigma = sigma)
    
    # Convert the smoothed array back to data.table
    idx_smoothed <- which(!is.na(arr_smoothed), arr.ind = TRUE)
    dt_smoothed <- data.table::data.table(
        x_idx = idx_smoothed[, 1],
        y_idx = idx_smoothed[, 2],
        z_idx = idx_smoothed[, 3],
        intensity = arr_smoothed[idx_smoothed]
    )
    
    # Map indices back to x, y, and wavenumber
    dt_smoothed[, x := x_vals[x_idx]]
    dt_smoothed[, y := y_vals[y_idx]]
    dt_smoothed[, wavenumber := z_vals[z_idx]]
    
    # Map back to 'col_id' using coordinates
    # Create a lookup table for (x, y) to col_id
    coord_to_colid <- coords[, .(x, y, col_id)]
    setkey(coord_to_colid, x, y)
    
    # Merge col_id into dt_smoothed
    dt_smoothed <- merge(dt_smoothed, coord_to_colid, by = c("x", "y"), all.x = TRUE)
    
    # Reshape data to wide format
    dt_wide <- data.table::dcast(
        dt_smoothed,
        wavenumber ~ col_id,
        value.var = "intensity"
    )
    
    # Ensure the columns are in the same order as original spectra
    spectra_smoothed <- dt_wide[, names(spectra), with = FALSE]
    
    # Create new OpenSpecy object with smoothed spectra
    x_smoothed <- as_OpenSpecy(
        x = dt_wide$wavenumber,
        spectra = spectra_smoothed,
        metadata = x$metadata,
        session_id = TRUE
    )
    
    return(x_smoothed)
}