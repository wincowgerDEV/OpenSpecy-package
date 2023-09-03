#' @title Sample Raman spectrum
#'
#' @description
#' Raman spectrum of high-density polyethylene (HDPE) provided by
#' Horiba Scientific.
#'
#' @format
#' An threepart list of class \code{\link{OpenSpecy}} containing:
#' \tabular{ll}{
#' \code{wavenumber}: \tab spectral wavenumbers \[1/cm\] (vector of 964 rows) \cr
#' \code{spectra}: \tab absorbance values [-]
#'   (a \code{\link[data.table]{data.table}} with 964 rows and 1 column) \cr
#' \code{metadata}: \tab spectral metadata \cr
#' }
#'
#' @examples
#' data(raman_hdpe)
#' print(raman_hdpe)
#'
#' @author
#' Zacharias Steinmetz, Win Cowger
#'
#' @references
#' Cowger W, Gray A, Christiansen SH, De Frond H, Deshpande AD, Hemabessiere L,
#' Lee E, Mill L, et al. (2020). “Critical Review of Processing and
#' Classification Techniques for Images and Spectra in Microplastic Research.”
#' \emph{Applied Spectroscopy}, \strong{74}(9), 989–1010.
#' \doi{10.1177/0003702820929064}.
#'
#' @docType data
#' @keywords data
#' @name raman_hdpe
NULL
