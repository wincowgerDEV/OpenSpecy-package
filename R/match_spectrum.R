#' @title Match spectra with reference library
#'
#' @description
#' This function will compare a spectrum to a spectral library formatted with the Open Specy standard and report the best match using the Pearson correlation coefficient.
#'
#' @details
#' This routine will first match the spectrum you want to identify to the wavenumbers present in the spectral library.
#' Once the spectra are aligned, it computes the pearson corellation coefficient between the spectrum you want to identify and all spectra in the library.
#' Lastly, it returns a table with the Pearson correlation coefficient values and all metadata for the top spectral matches.
#' If using the Open Specy library, all intensity values are in absorbance so your spectra should also be in absorbance units. If you need to convert your spectrum, see \code{adjust_intensity()}
#'
#' @param x Wavenumber column
#' @param y Intensity column
#' @param formula formula
#' @param data Data you want to identify.
#' @param library Library you want to compare against.
#' @param which which
#' @param type type
#' @param range This should be all possible wavenumber values from your spectral library.
#' @param top_n Number of top matches that you want to be returned.
#' @param \ldots ...
#'
#' @seealso
#' seealso
#'
#' @examples
#' \dontrun{
#' data("raman_hdpe")
#'
#' get_lib("raman")
#' spec_lib <- load_lib("raman")
#'
#' match_spectrum(raman_hdpe, spec_lib, "raman", "full")
#' }
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom stats approx cor
#' @importFrom dplyr inner_join rename group_by mutate group_by ungroup summarize select arrange desc top_n
#'
#' @export
match_spectrum <- function(x, ...) {
  UseMethod("match_spectrum")
}

#' @rdname match_spectrum
#'
#' @export
match_spectrum.formula <- function(formula, data = NULL, ...) {
  if (missing(formula) || (length(formula) != 3L) || (length(attr(terms(formula[-2L]),
                                                                  "term.labels")) != 1L))
    stop("'formula' missing or incorrect")

  mf <- model.frame(formula, data)
  lst <- as.list(mf)
  names(lst) <- c("y", "x")

  do.call("match_spectrum", c(lst, list(...)))
}

#' @rdname match_spectrum
#'
#' @export
match_spectrum.data.frame <- function(x, ...) {
  if (!all(c("wavenumber", "intensity") %in% names(x)))
    stop("'data' must contain 2 columns named 'wavenumber' and 'intensity'")

  do.call("match_spectrum", list(x$wavenumber, x$intensity, ...))
}

#' @rdname match_spectrum
#'
#' @export
match_spectrum.default <- function(x, y, library, which = NULL, type = "full",
                                   range = seq(0, 6000, 0.1), top_n = 100, ...) {
  if(type == "full") type <- "library"

  lib <- library[[which]][[type]]
  meta <- library[[which]][["metadata"]]

  wls <- range[range >= min(x) & range <= max(x)]

  m <- lib %>%
    inner_join(dplyr::rename(data.frame(approx(x, y, xout = wls, rule = 2,
                                               method = "linear", ties = mean)),
                             "wavenumber" = .data$x), by = "wavenumber") %>%
    dplyr::rename("BaselineRemove" = .data$y) %>%
    group_by(.data$group, .data$sample_name) %>%
    mutate(BaselineRemove = .data$BaselineRemove - min(.data$BaselineRemove)) %>%
    ungroup() %>%
    mutate(BaselineRemove = make_relative(.data$BaselineRemove)) %>%
    group_by(.data$sample_name) %>%
    dplyr::summarize(rsq = cor(.data$intensity, .data$BaselineRemove)) %>%
    top_n(top_n, .data$rsq) %>%
    inner_join(select(meta, -.data$rsq), by = "sample_name") %>%
    select(.data$sample_name, .data$SpectrumIdentity, .data$rsq, .data$Organization) %>%
    arrange(desc(.data$rsq)) %>%
    mutate(rsq = round(.data$rsq, 2))

  message("Visit openspecy.org to share your spectrum with the Open Specy ",
          "community and improve the spectral library.")
  return(m)
}
