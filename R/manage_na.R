#' @rdname manage_na
#' @title Ignore or remove NA intensities
#'
#' @description
#' Sometimes you want to keep or remove NA values in intensities to allow for
#' spectra with varying shapes to be analyzed together or maintained in a single
#' Open Specy object. For \code{type = "ignore"}, spectra sharing the same
#' missing-value boundaries are processed together, valid source-specific ranges
#' are retained, and attributes returned by the processing function are copied
#' back to the full object.
#'
#' @param x a numeric vector or an \R OpenSpecy object.
#' @param lead_tail_only logical whether to only look at leading adn tailing values.
#' @param ig character vector, values to ignore.
#' @param fun the name of the function you want run, this is only used if the "ignore" type is chosen.
#' @param type character of either "ignore" or "remove".
#' @param \ldots further arguments passed to \code{fun}.
#'
#' @return
#' \code{manage_na()} return logical vectors of NA locations (if vector provided) or an
#' \code{OpenSpecy} object with ignored or removed NA values.
#'
#' @examples
#' manage_na(c(NA, -1, NA, 1, 10))
#' manage_na(c(NA, -1, NA, 1, 10), lead_tail_only = FALSE)
#' manage_na(c(NA, 0, NA, 1, 10), lead_tail_only = FALSE, ig = c(NA,0))
#' data(raman_hdpe)
#' raman_hdpe$spectra[1:10, 1] <- NA
#'
#' #would normally return all NA without na.rm = TRUE but doesn't here.
#' manage_na(raman_hdpe, fun = make_rel)
#'
#' #will remove the first 10 values we set to NA
#' manage_na(raman_hdpe, type = "remove")
#'
#' @author
#' Win Cowger, Zacharias Steinmetz
#'
#' @seealso
#' \code{OpenSpecy} object to be matched with a reference library
#' \code{fill_spec()} can be used to fill NA values in Open Specy objects.
#' \code{restrict_range()} can be used to restrict spectral ranges in other ways than removing NAs.
#'
#' @export
manage_na <- function(x, ...) {
  UseMethod("manage_na")
}

#' @rdname manage_na
#' @export
manage_na.default <- function(x, lead_tail_only = TRUE, ig = c(NA), ...) {

  if(all(is.na(x)))
    stop("All intensity values are NA, cannot remove or ignore with manage na.")

  if(lead_tail_only) {
    na_positions <- logical(length(x))
    if(x[1] %in% ig) {
      criteria = T
      y = 1
      while(criteria) {
        if(x[y] %in% ig) na_positions[y] <- T
        y = y + 1
        criteria = x[y] %in% ig
      }
    }
    if(x[length(x)] %in% ig) {
      criteria = T
      y = length(x)
      while(criteria){
        if(x[y] %in% ig) na_positions[y] <- T
        y = y - 1
        criteria = x[y] %in% ig
      }
    }
  }
  else{
    na_positions <- x %in% ig
  }

  return(na_positions)
}

#' @rdname manage_na
#' @export
manage_na.OpenSpecy <- function(x, lead_tail_only = TRUE, ig = c(NA), fun,
                                type = "ignore", ...) {
  x <- as_OpenSpecy(x)

  ignored_info <- .spectra_ignore_info(
    x$spectra,
    lead_tail_only = lead_tail_only,
    ig = ig
  )
  ignored <- ignored_info$ignored

  if(type == "ignore"){
    if (missing(fun)) {
      stop("'fun' must be supplied when type = 'ignore'", call. = FALSE)
    }
    if (!any(ignored)) {
      return(do.call(fun, c(list(x), list(...))))
    }
    if (any(!ignored_info$has_valid)) {
      stop("All intensity values are NA, cannot remove or ignore with manage na.",
           call. = FALSE)
    }

    if (lead_tail_only) {
      mask_keys <- paste(ignored_info$first, ignored_info$last, sep = ":")
    } else {
      mask_keys <- vapply(seq_len(ncol(ignored)), function(i) {
        paste(which(ignored[, i]), collapse = ",")
      }, FUN.VALUE = character(1))
    }

    for (cols in split(seq_len(ncol(x$spectra)), mask_keys)) {
      keep <- if (lead_tail_only) {
        seq.int(ignored_info$first[cols[1L]], ignored_info$last[cols[1L]])
      } else {
        !ignored[, cols[1L]]
      }
      if (!any(keep)) {
        stop("All intensity values are NA, cannot remove or ignore with manage na.",
             call. = FALSE)
      }

      reduced <- x
      reduced$wavenumber <- x$wavenumber[keep]
      reduced$spectra <- x$spectra[keep, cols, drop = FALSE]
      reduced$metadata <- data.table::copy(x$metadata[cols])
      processed <- do.call(fun, c(list(reduced), list(...)))

      if (!identical(processed$wavenumber, reduced$wavenumber) ||
          !identical(dim(processed$spectra), dim(reduced$spectra))) {
        stop("Functions used with manage_na(type = 'ignore') must preserve ",
             "the wavenumber axis and spectra dimensions", call. = FALSE)
      }

      x$spectra[keep, cols] <- processed$spectra
      x <- .copy_open_specy_attributes(x, processed)
    }
  }

  if(type == "remove"){
    consistent <- rowSums(ignored) == 0
    x$wavenumber <- x$wavenumber[consistent]
    x$spectra <- x$spectra[consistent, , drop = FALSE]
  }

  return(x)
}

.spectra_ignore_info <- function(spectra, lead_tail_only = TRUE, ig = c(NA)) {
  raw_ignored <- .ignored_value_matrix(spectra, ig)

  if (!lead_tail_only) {
    return(list(
      ignored = raw_ignored,
      has_valid = colSums(!raw_ignored) > 0L,
      first = rep(NA_integer_, ncol(spectra)),
      last = rep(NA_integer_, ncol(spectra))
    ))
  }

  leading <- raw_ignored
  trailing <- raw_ignored
  if (nrow(raw_ignored) > 1L) {
    for (i in 2:nrow(raw_ignored)) {
      leading[i, ] <- leading[i, ] & leading[i - 1L, ]
    }
    for (i in (nrow(raw_ignored) - 1L):1L) {
      trailing[i, ] <- trailing[i, ] & trailing[i + 1L, ]
    }
  }

  has_valid <- colSums(!raw_ignored) > 0L
  first <- colSums(leading) + 1L
  last <- nrow(raw_ignored) - colSums(trailing)
  first[!has_valid] <- NA_integer_
  last[!has_valid] <- NA_integer_

  list(
    ignored = leading | trailing,
    has_valid = has_valid,
    first = as.integer(first),
    last = as.integer(last)
  )
}

.ignored_value_matrix <- function(spectra, ig = c(NA)) {
  ignored <- matrix(FALSE, nrow = nrow(spectra), ncol = ncol(spectra))
  if (anyNA(ig)) ignored <- is.na(spectra)

  values <- ig[!is.na(ig)]
  if (length(values) > 0L) {
    for (value in values) {
      matched <- spectra == value
      matched[is.na(matched)] <- FALSE
      ignored <- ignored | matched
    }
  }
  ignored
}

.copy_open_specy_attributes <- function(x, source) {
  updated_attributes <- setdiff(names(attributes(source)), c("names", "class"))
  for (nm in updated_attributes) {
    attr(x, nm) <- attr(source, nm)
  }
  x
}
