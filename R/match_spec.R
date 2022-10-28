#' @title Match spectra with reference library
#'
#' @description
#' \code{match_spec()} will compare a spectrum to a spectral library formatted
#' with the Open Specy standard and report the best match using the Pearson
#' correlation coefficient.
#' \code{find_spec()} makes it easy to retrieve single spectra and metadata
#' from the Open Specy reference library.
#'
#' @details
#' This routine will match the spectrum you want to identify to the wavenumbers
#' present in the spectral library. Once the spectra are aligned, it computes
#' the Pearson correlation coefficient between the spectrum you want to
#' identify and all spectra in the library (see \code{\link[stats]{cor}}).
#' The function returns a table with the Pearson correlation coefficient values
#' and all metadata for the top spectral matches.
#' If using the Open Specy library, all intensity values are in absorbance, so
#' your spectra should also be in absorbance units. If you need to convert your
#' spectrum, use \code{\link{adj_intens}()}.
#'
#' @param x a numeric vector containing the spectral wavenumbers; alternatively
#' a data frame containing spectral data as \code{"wavenumber"} and
#' \code{"intensity"} can be supplied.
#' @param y a numeric vector containing the spectral intensities.
#' @param formula an object of class '\code{\link[stats]{formula}}' of the form
#' \code{intensity ~ wavenumber}.
#' @param data a data frame containing the variables in \code{formula}.
#' @param library reference library you want to compare against.
#' @param which a character string specifying which library to match,
#' \code{"raman"} or \code{"ftir"}.
#' @param type a character string specifying whether the \code{"full"} spectrum
#' should be matched or spectrum \code{"peaks"} only. \code{"metadata"} is
#' needed to browser spectra with \code{find_spec()}.
#' @param range this should be all possible wavenumber values from your spectral
#' library.
#' @param top_n number of top matches that you want to be returned.
#' @param subset logical expression indicating elements or rows to search for;
#' see \code{\link[base]{subset}()} for details.
#' @param cols columns to retrieve from the Open Specy reference library;
#' columns containing no or missing values are automatically removed.
#' @param \ldots further arguments passed to the submethods.
#'
#' @examples
#' \dontrun{
#' data("raman_hdpe")
#'
#' get_lib("raman")
#' spec_lib <- load_lib("raman")
#'
#' match_spec(raman_proc, library = spec_lib, which = "raman")
#'
#' find_spec(sample_name == 5381, library = spec_lib, which = "raman")
#' }
#'
#' @return
#' \code{match_spec()} returns a data frame with the \code{top_n} material
#' matches, their Pearson's r value, and the organization they were provided by.
#' \code{find_spec()} returns a data frame with the spectral raw data or
#' metadata of a specific reference spectrum.
#'
#' @author
#' Win Cowger, Zacharias Steinmetz
#'
#' @seealso
#' \code{\link{adj_intens}()} converts spectra;
#' \code{\link{get_lib}()} retrieves the Open Specy reference library;
#' \code{\link{load_lib}()} loads the Open Specy reference library into an \R
#' object of choice

correlate_spectra <- function(data, library, ...){
    prep_data = data$spectra[search_wavenumbers %in% library$wavenumbers,][,lapply(.SD, make_rel, na.rm = T)][,lapply(.SD, mean_replace)]
    prep_library = library$spectra[std_wavenumbers %in% data$wavenumbers,][,lapply(.SD, make_rel, na.rm = T)][,lapply(.SD, mean_replace)]
    
    cor(prep_data, 
        prep_library,
        ...)
}

mean_replace <- function(intensity, na.rm = T){
    fifelse(is.na(intensity), mean(intensity, na.rm = na.rm), intensity)
}

get_all_metadata <- function(sample_names, correlation_rsq, metadata) {
    left_join(data.table(sample_name = sample_names, rsq = correlation_rsq), metadata) %>%
        filter(!is.na(rsq)) %>%
        arrange(desc(rsq)) %>%
        mutate(rsq = round(rsq, 2)) 
}

single_metadata <- function(all_metadata, selection){
    all_metadata[selection,] %>%
        select(where(~!any(is_empty(.))))
}

is_empty <- function(x, first.only = TRUE, all.na.empty = TRUE) {
    # do we have a valid vector?
    if (!is.null(x)) {
        # if it's a character, check if we have only one element in that vector
        if (is.character(x)) {
            # characters may also be of length 0
            if (length(x) == 0) return(TRUE)
            # else, check all elements of x
            zero_len <- nchar(x) == 0
            # return result for multiple elements of character vector
            if (first.only) {
                zero_len <- .is_true(zero_len[1])
                if (length(x) > 0) x <- x[1]
            } else {
                return(unname(zero_len))
            }
            # we have a non-character vector here. check for length
        } else if (is.list(x)) {
            x <- purrr::compact(x)
            zero_len <- length(x) == 0
        } else {
            zero_len <- length(x) == 0
        }
    }
    
    any(is.null(x) || zero_len || (all.na.empty && all(is.na(x))))
}

.is_true <- function(x) {
    is.logical(x) && length(x) == 1L && !is.na(x) && x
}

max_cor <- function(correlation_matrix, na.rm = T){
    round(apply(correlation_matrix, 2, function(x) max(x, na.rm = na.rm)), 2)
}

max_cor_id <- function(correlation_matrix, library){
    colnames(library)[apply(correlation_matrix, 2, function(x) which.max(x))]
}


#'
#' @importFrom rlang .data
#' @importFrom stats approx cor
#' @importFrom magrittr %>%
#' @importFrom dplyr inner_join mutate group_by ungroup summarize select arrange desc top_n
#' @export
match_spec <- function(x, ...) {
  UseMethod("match_spec")
}

#' @rdname match_spec
#'
#' @export
match_spec.formula <- function(formula, data = NULL, ...) {
  if (missing(formula) || (length(formula) != 3L) ||
      (length(attr(terms(formula[-2L]), "term.labels")) != 1L))
    stop("'formula' missing or incorrect", call. = F)

  mf <- model.frame(formula, data)
  lst <- as.list(mf)
  names(lst) <- c("y", "x")

  do.call("match_spec", c(lst, list(...)))
}

#' @rdname match_spec
#'
#' @export
match_spec.data.frame <- function(x, ...) {
  if (!all(c("wavenumber", "intensity") %in% names(x)))
    stop("'data' must contain 2 columns named 'wavenumber' and 'intensity'",
         call. = F)

  do.call("match_spec", list(x$wavenumber, x$intensity, ...))
}

#' @rdname match_spec
#'
#' @export
match_spec.default <- function(x, y, library, which = NULL, type = "full",
                               range = seq(0, 6000, 0.1), top_n = 100, ...) {
  if(type == "full") type <- "library"

  lib <- library[[which]][[type]]
  meta <- library[[which]][["metadata"]]

  wls <- range[range >= min(x) & range <= max(x)]
  aprx <- approx(x, y, xout = wls, rule = 2, method = "linear", ties = mean) %>%
    data.frame()
  names(aprx) <- c("wavenumber", "baseline_remove")

  m <- lib %>%
    inner_join(aprx, by = "wavenumber") %>%
    group_by(.data$group, .data$sample_name) %>%
    mutate(baseline_remove = .data$baseline_remove -
             min(.data$baseline_remove)) %>%
    ungroup() %>%
    mutate(baseline_remove = make_rel(.data$baseline_remove)) %>%
    group_by(.data$sample_name) %>%
    summarize(rsq = cor(.data$intensity, .data$baseline_remove)) %>%
    top_n(top_n, .data$rsq) %>%
    inner_join(select(meta, -.data$rsq), by = "sample_name") %>%
    select(.data$sample_name, .data$spectrum_identity, .data$rsq,
           .data$organization) %>%
    arrange(desc(.data$rsq)) %>%
    mutate(rsq = round(.data$rsq, 2))

  message("Visit openspecy.org to share your spectrum with the Open Specy ",
          "community and improve the spectral library.")
  return(m)
}

#' @rdname match_spec
#'
#' @export
find_spec <- function(subset, library, which = NULL, type = "metadata",
                      cols = c("spectrum_identity", "organization",
                               "contact_info", "spectrum_type",
                               "instrument_used", "instrument_accessories",
                               "instrument_mode", "laser_light_used",
                               "total_acquisition_time_s",
                               "number_of_accumulations",
                               "level_of_confidence_in_identification",
                               "cas_number", "material_producer",
                               "material_purity", "material_form",
                               "material_quality", "spectral_resolution",
                               "data_processing_procedure",
                               "other_information", "sample_name",
                               "wavenumber", "intensity", "group"),
                      ...) {

  if (type == "full") type <- "library"

  df <- data.frame(library[[which]][[type]])
  e <- substitute(subset)

  if (!is.call(e)) stop("subset needs to be a logical expression", call. = F)

  r <- eval(e, df, parent.frame())
  c <- cols[cols %in% names(df)]
  out <- df[r, c]

  out[,colSums(is.na(out)) < nrow(out) &
        colSums(out == "") < nrow(out)]
}
