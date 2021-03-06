#' @title Match spectra with reference library
#'
#' @description
#' desc
#'
#' @param spectrum spectrum
#' @param library library
#' @param which which
#' @param type type
#' @param range range
#' @param col_names col_names
#' @param top_n top_n
#' @param \ldots ...
#'
#' @seealso
#' seealso
#'
#' @examples
#' data("raman_hdpe")
#'
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom stats approx cor
#' @importFrom dplyr inner_join rename group_by mutate group_by ungroup summarize select arrange desc
#' @export
match_spectrum <- function(spectrum, library, which = NULL, type = "full",
                           range = seq(0, 6000, 0.1), col_names = NULL, top_n = 100,
                           ...) {
  # Try to guess column names
  if (is.null(col_names))
    col_names <- c(
      names(spectrum)[grep("wav*", ignore.case = T, names(spectrum))][1L],
      names(spectrum)[grep("(transmit*)|(reflect*)|(abs*)|(intens*)",
                           ignore.case = T, names(spectrum))][1L]
    )
  if(type == "full") type <- "library"

  spc <- spectrum[col_names]
  lib <- library[[which]][[type]]
  meta <- library[[which]][["metadata"]]

  wls <- range[range >= min(spc[, 1]) & range <= max(spc[, 1])]

  m <- lib %>%
    inner_join(dplyr::rename(data.frame(approx(spc[, 1], spc[, 2], xout = wls,
                                               rule = 2, method = "linear", ties=mean)),
                             "Wavelength" = .data$x), by = "Wavelength") %>%
    dplyr::rename("BaselineRemove" = .data$y) %>%
    group_by(.data$group, .data$SampleName) %>%
    mutate(BaselineRemove = .data$BaselineRemove - min(.data$BaselineRemove)) %>%
    ungroup() %>%
    mutate(BaselineRemove = make_relative(.data$BaselineRemove)) %>%
    group_by(.data$SampleName) %>%
    dplyr::summarize(rsq = cor(.data$Intensity, .data$BaselineRemove)) %>%
    top_n(top_n, .data$rsq) %>%
    inner_join(select(meta, -.data$rsq), by = "SampleName") %>%
    select(.data$SampleName, .data$SpectrumIdentity, .data$rsq, .data$Organization) %>%
    arrange(desc(.data$rsq)) %>%
    mutate(rsq = round(.data$rsq, 2))

  return(m)
}
