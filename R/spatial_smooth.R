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
#' matches the column names in the `spectra` matrix.
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
#' @importFrom mmand gaussianSmooth
#' @export
spatial_smooth <- function(x, sigma = c(1, 1, 1), ...) {
    # Check that x is an OpenSpecy object
    if (!inherits(x, "OpenSpecy")) {
        stop("x must be an OpenSpecy object.", call. = FALSE)
    }
    x <- as_OpenSpecy(x)
    
    # Check that metadata contains 'x', 'y', and 'col_id'
    if (is.null(x$metadata) || !all(c("x", "y", "col_id") %in% names(x$metadata))) {
        stop("The OpenSpecy object must have 'x', 'y', and 'col_id' columns in its metadata.", call. = FALSE)
    }
    
    #Avoid data.table notes
    col_id <- y <- NULL
    
    # Extract wavenumbers and spectra
    wavenumbers <- x$wavenumber
    spectra <- x$spectra
    
    # Extract coordinates and ensure they are numeric
    coords <- x$metadata[, list(col_id = col_id, x = x, y = y)]
    #coords[, `:=`(x = as.numeric(x), y = as.numeric(y))]
    
    # Ensure that 'col_id's in metadata match column names in 'spectra'
    if (!all(coords$col_id %in% colnames(spectra))) {
        stop("Not all 'col_id's in metadata match column names in spectra.", call. = FALSE)
    }
    coords <- coords[match(colnames(spectra), coords$col_id)]
    
    # Get unique x, y, and wavenumber values
    x_vals <- sort(unique(coords$x))
    y_vals <- sort(unique(coords$y))
    z_vals <- wavenumbers  # wavenumbers are already sorted
    
    nx <- length(x_vals)
    ny <- length(y_vals)
    nz <- length(z_vals)
    
    # Map x, y, and wavenumber to indices
    x_idx <- match(coords$x, x_vals)
    y_idx <- match(coords$y, y_vals)
    z_idx <- seq_len(nz)
    
    # Initialize empty array
    arr <- array(NA_real_, dim = c(nx, ny, nz))
    
    # Fill the array with intensity values
    idx <- cbind(rep(x_idx, each = nz),
                 rep(y_idx, each = nz),
                 rep(z_idx, times = ncol(spectra)))
    arr[idx] <- as.vector(spectra)
    
    # Apply Gaussian smoothing (sigma for x and y; 0 for wavenumber dimension)
    arr_smoothed <- mmand::gaussianSmooth(arr, sigma = sigma)
    
    spectra_smoothed <- matrix(NA_real_, nrow = nz, ncol = ncol(spectra),
                               dimnames = list(NULL, colnames(spectra)))
    for (i in seq_len(ncol(spectra))) {
        spectra_smoothed[, i] <- arr_smoothed[x_idx[i], y_idx[i], ]
    }
    
    # Create new OpenSpecy object with smoothed spectra
    x_smoothed <- as_OpenSpecy(
        x = z_vals,
        spectra = spectra_smoothed,
        metadata = x$metadata,
        session_id = TRUE
    )
    
    return(x_smoothed)
}
