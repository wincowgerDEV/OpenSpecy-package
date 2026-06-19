#' @rdname manage_spec
#' @title Manage spectral objects
#'
#' @description
#' \code{c_spec()} concatenates \code{OpenSpecy} objects.
#' \code{sample_spec()} samples spectra from an \code{OpenSpecy} object.
#' \code{merge_map()} merge two \code{OpenSpecy} objects from spectral maps.
#' @param x a list of \code{OpenSpecy} objects or of file paths.
#' @param range a numeric providing your own wavenumber range,
#' \code{"full"} to use the widest range represented by any supplied spectrum,
#' or \code{"common"} to use only their overlapping range. \code{NULL} requires
#' identical wavenumbers. The default is \code{"full"}.
#' @param res resolution of the output wavenumbers. The default of 6 is intended
#' for reference-library identification workflows.
#' @param size the number of spectra to sample.
#' @param prob probabilities to use for the sampling.
#' @param origins a list with 2 value vectors of x y coordinates for the offsets of each image. 
#' @param \ldots further arguments passed to submethods.
#'
#' @return
#' \code{c_spec()} and \code{sample_spec()} return \code{OpenSpecy} objects.
#'
#' @examples
#' # Concatenating spectra
#' spectra <- lapply(c(read_extdata("raman_hdpe.csv"),
#'                     read_extdata("ftir_ldpe_soil.asp")), read_any)
#' full <- c_spec(spectra)
#' common <- c_spec(spectra, range = "common", res = 6)
#' range <- c_spec(spectra, range = c(1000, 2000), res = 6)
#'
#' # Sampling spectra
#' tiny_map <- read_any(read_extdata("CA_tiny_map.zip"))
#' sampled <- sample_spec(tiny_map, size = 3)
#'
#' @author
#' Zacharias Steinmetz, Win Cowger
#'
#' @seealso
#' \code{\link[OpenSpecy]{conform_spec}()} for conforming wavenumbers
#'
#' @importFrom data.table data.table as.data.table fread rbindlist
#'
#' @export
c_spec <- function(x, ...) {
  UseMethod("c_spec")
}

#' @rdname manage_spec
#'
#' @export
c_spec.default <- function(x, ...) {
  stop("object 'x' needs to be a list of 'OpenSpecy' objects")
}

#' @rdname manage_spec
#'
#' @export
c_spec.OpenSpecy <- function(x, ...) {
  message("object 'x' needs to be a list of 'OpenSpecy' objects; ",
          "nothing to concatenate, returning 'x'")

  return(x)
}

#' @rdname manage_spec
#'
#' @export
c_spec.list <- function(x, range = "full", res = 6, ...) {
  if(!is_OpenSpecy(x[[1]])){
     x <- lapply(x, read_any)
  }

  x <- lapply(x, as_OpenSpecy)

  if(!all(vapply(x, function(y) {inherits(y, "OpenSpecy")}, FUN.VALUE = T)))
    stop("object 'x' needs to be a list of 'OpenSpecy' objects", call. = F)

  if(!is.null(range)) {
    if(is.numeric(range)) {
      wn <- range
    }
    else if(length(range) == 1L && range %in% c("common", "full")) {
      pmin <- vapply(x, function(y) min(y$wavenumber), FUN.VALUE = numeric(1))
      pmax <- vapply(x, function(y) max(y$wavenumber), FUN.VALUE = numeric(1))

      if(range == "common" &&
         (any(max(pmin) > pmax) | any(min(pmax) < pmin)))
        stop("data points need to overlap in their ranges", call. = F)

      wn <- if (range == "common") {
        c(max(pmin), min(pmax))
      } else {
        c(min(pmin), max(pmax))
      }
    } else {
      stop("If range is specified it should be numeric, 'full', or 'common'",
           call. = FALSE)
    }
    x <- lapply(x, conform_spec, range = wn, res = res,
                allow_na = identical(range, "full") || is.numeric(range))
  }

  unlisted <- unlist(unname(x), recursive = F)
  list <- tapply(unlisted, names(unlisted), unname)

  if(length(unique(vapply(list$wavenumber, length, FUN.VALUE = numeric(1)))) > 1 &
     is.null(range)) {
    stop("wavenumbers need to be identical between spectra; specify how; use ",
         "'range' to specify how wavenumbers should be merged", call. = F)
  }

  spectra <- do.call(cbind, list$spectra)
  colnames(spectra) <- make.unique(unlist(lapply(list$spectra, colnames)),
                                   sep = ".")

  metadata <- rbindlist(list$metadata, fill = TRUE)
  metadata <- metadata[, setdiff(names(metadata), c("x", "y")), with = FALSE]
  attribute_names <- c("intensity_unit", "derivative_order", "baseline",
                       "spectra_type")
  object_attributes <- lapply(attribute_names, function(nm) {
    values <- lapply(x, attr, which = nm)
    if (all(vapply(values[-1L], identical, logical(1), values[[1L]]))) {
      values[[1L]]
    } else {
      NULL
    }
  })
  names(object_attributes) <- attribute_names

  as_OpenSpecy(x = list$wavenumber[[1]],
               spectra = spectra,
               metadata = metadata,
               attributes = object_attributes)
}


#' @rdname manage_spec
#'
#' @export
sample_spec <- function(x, ...) {
  UseMethod("sample_spec")
}

#' @rdname manage_spec
#'
#' @export
sample_spec.default <- function(x, ...) {
  stop("object 'x' needs to be of class 'OpenSpecy'")
}

#' @rdname manage_spec
#'
#' @export
sample_spec.OpenSpecy <- function(x, size = 1, prob = NULL, ...) {
  x <- as_OpenSpecy(x)
  # replace = false is mandatory currently because we don't have a way to
  # rename and recoordinate duplicates.
  cols <- sample(1:ncol(x$spectra), size = size, replace = FALSE, prob = prob, ...)

  filter_spec(x, cols)
}


#' @rdname manage_spec
#'
#' @export
merge_map <- function(x, ...) {
    UseMethod("merge_map")
}

#' @rdname manage_spec
#'
#' @export
merge_map.default <- function(x, ...) {
    stop("object 'x' needs to be a list of 'OpenSpecy' objects or file paths")
}

#' @rdname manage_spec
#'
#' @export
merge_map.OpenSpecy <- function(x, ...) {
    stop("object 'x' needs to be a list of 'OpenSpecy' objects or file paths")
}

#' @rdname manage_spec
#'
#' @export
merge_map.list <- function(x, origins = NULL, ...) {
    
    if(!is_OpenSpecy(x[[1]])){
        map <- lapply(x, read_any)
    }
    
    else{
        map <- x
    }

    map <- lapply(map, as_OpenSpecy)
    
    if(is.null(origins)){
        origin = lapply(map, function(x) unique(x$metadata$description))
        pix_size = lapply(map, function(x) unique(x$metadata$`pixel size`)) 
        pixel_size = vapply(pix_size, function(x) as.numeric(gsub("(\\{)|(\\})|(,.*)", "",x))*10^6, FUN.VALUE = numeric(1))
        originx = vapply(origin, function(x) gsub(",.*", "", gsub(".*X=", "",  x)) |> as.numeric(), FUN.VALUE = numeric(1))
        originy = vapply(origin, function(x) gsub(".*Y=", "",  x) |> as.numeric(), FUN.VALUE = numeric(1))
        xoffset = as.integer((originx-min(originx))/pixel_size)
        yoffset = as.integer((originy-min(originy))/pixel_size)
    }
    
    else{
        if(!is.list(origins)) stop("origins must be a list of 2 value x y vectors or NULL if trying to automate")
        xoffset = vapply(origins, function(x) x[1], FUN.VALUE = numeric(1))
        yoffset = vapply(origins, function(x) x[2], FUN.VALUE = numeric(1))
    }
    
    if(!is.numeric(xoffset)) stop("Origin extraction failed, the hdr file must have description metadata or you must provide numeric values in your list.")
    
    for(x in 1:length(map)){
        map[[x]]$metadata$x <- map[[x]]$metadata$x + xoffset[x]
        map[[x]]$metadata$y <- map[[x]]$metadata$y + yoffset[x]
    }
    
    unlisted <- unlist(unname(map), recursive = F)
    
    list <- tapply(unlisted, names(unlisted), unname)
    
    spectra <- do.call(cbind, list$spectra)
    colnames(spectra) <- make.unique(unlist(lapply(list$spectra, colnames)),
                                     sep = ".")

    map <- as_OpenSpecy(x = list$wavenumber[[1]],
                        spectra = spectra,
                        metadata = rbindlist(list$metadata, fill = T))

    ids <- paste(map$metadata$x, map$metadata$y, sep = ",")
    map$metadata$sample_name <- ids
    uids <- unique(ids)
    collapsed <- matrix(NA_real_, nrow = nrow(map$spectra), ncol = length(uids),
                        dimnames = list(NULL, uids))
    for (i in seq_along(uids)) {
      collapsed[, i] <- matrixStats::rowMedians(
        map$spectra[, ids == uids[i], drop = FALSE],
        na.rm = TRUE
      )
    }
    map$spectra <- collapsed
    map$metadata <- map$metadata[match(uids, ids)]
    map$metadata$col_id <- colnames(map$spectra)
    map
}


