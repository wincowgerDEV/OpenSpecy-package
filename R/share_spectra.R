#' @rdname share_spectra
#'
#' @title Share data with the Open Specy community
#'
#' @description
#' This helper functions share spectra and metadata with the Open Specy
#' community.
#'
#' @param data a data frame containing the spectral data; columns should be
#' named \code{"wavenumber"} and \code{"intensity"}.
#' @param share \code{"system"} just keeps a local copy of the spectrum in the
#' Open Specy package directory at
#' \code{system.file("extdata", package = "OpenSpecy")} to be shared with us by
#' e-mail; if a correct API token exists, "dropbox" shares the spectrum with
#' the cloud.
#' @param \ldots further arguments passed to the submethods.
#'
#' @seealso
#' \code{\link{read_text}()}
#'
#' @examples
#' data("raman_hdpe")
#'
#' share_spectrum(raman_hdpe)
#'
#' @importFrom magrittr %>%
#' @importFrom digest digest
#' @importFrom utils write.csv
#'
#' @export
share_spectrum <- function(data, ...) {
  UseMethod("share_spectrum")
}

#' @rdname share_spectra
#'
#' @export
share_spectrum.default <- function(data, ...) {
  stop("object needs to be of class 'data.frame'")
}

#' @rdname share_spectra
#'
#' @export
share_spectrum.data.frame <- function(data, share = "system", ...) {
  id <- digest(data, algo = "md5")
  fn <- paste0(paste(human_timestamp(), id, sep = "_"), ".csv")

  ty <- "thank you for sharing"

  if (share == "system") {
    pkg <- system.file("extdata", package = "OpenSpecy")
    dir.create(file.path(pkg, "user_spectra"), showWarnings = F)

    fw <- file.path(pkg, "user_spectra", fn)
    write.csv(data, fw, row.names = FALSE, quote = TRUE)

    message(ty, ", use 'share_metadata()' to send us more information")
  } else if (share == "dropbox") {
    if (!requireNamespace("rdrop2"))
      stop("share = 'dropbox' requires package 'rdrop2'")

    fw <- file.path(tempdir(), fn)
    write.csv(data, fw, row.names = FALSE, quote = TRUE)

    rdrop2::drop_upload(fw, path = "Spectra", ...)
  } else {
    if (!dir.exists(share)) dir.create(share, showWarnings = F)
    fw <- file.path(share, fn)
    write.csv(data, fw, row.names = FALSE, quote = TRUE, ...)

    message(ty, "; please e-mail your spectra to Win Cowger <wincowger@gmail.com>")
  }

  invisible()
}
