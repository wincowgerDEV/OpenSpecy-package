#' @rdname manage_lib
#' @title Manage spectral libraries
#'
#' @description
#' These functions will import the spectral libraries from Open Specy if they
#' were not already downloaded.
#' The CRAN does not allow for deployment of large datasets so this was a
#' workaround that we are using to make sure everyone can easily get Open Specy
#' functionality running on their desktop.
#'
#' @details
#' \code{check_lib()} checks to see if the Open Specy reference library
#' already exists on the users computer.
#' \code{get_lib()} downloads the Open Specy library from OSF
#' (\doi{10.17605/OSF.IO/X7DPZ}).
#' \code{load_lib()} will load the library into the global environment for use
#' with the Open Specy functions.
#' \code{rm_lib()} removes the libraries from your computer.
#'
#' @param type library type to check/retrieve; defaults to
#' \code{c("derivative", "nobaseline", "raw", "mediod", "model")} which reads
#' everything.
#' @param node the OSF node to be retrieved; should be \code{"x7dpz"} unless you
#' maintain your own OSF node with spectral libraries.
#' @param path where to save or look for local library files; defaults to
#' \code{"system"} pointing to
#' \code{system.file("extdata", package = "OpenSpecy")}.
#' @param conflicts determines what happens when a file with the same name
#' exists at the specified destination. Can be one of the following (see
#' \code{\link[osfr]{osf_download}()} for details):
#' \itemize{
#'   \item{"error"}{ throw an error and abort the file transfer operation.}
#'   \item{"skip"}{ skip the conflicting file(s) and continue transferring the
#'   remaining files.}
#'   \item{"overwrite" (default)}{ replace the existing file with the
#'   transferred copy.}
#' }
#' @param condition determines if \code{check_lib()} should warn
#' (\code{"warning"}, the default) or throw and error (\code{"error"}).
#' @param \ldots further arguments passed to \code{\link[osfr]{osf_download}()}.
#'
#' @return
#' \code{check_lib()} and \code{get_lib()} return messages only;
#' \code{load_lib()} returns an \code{OpenSpecy} object containing the
#' respective spectral reference library.
#'
#' @examples
#' \dontrun{
#' check_lib("derivative")
#' get_lib("derivative")
#'
#' spec_lib <- load_lib("derivative")
#' }
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
#' Cowger, W (2021). “Library data.” \emph{OSF}. \doi{10.17605/OSF.IO/X7DPZ}.
#'
#' @export
check_lib <- function(type = c("derivative", "nobaseline", "raw", "mediod",
                               "model"),
                      path = "system", condition = "warning") {

  lp <- ifelse(path == "system",
               system.file("extdata", package = "OpenSpecy"),
               path)

  .chkf(type, path = lp, condition = condition)

  invisible()
}

#' @rdname manage_lib
#'
#' @importFrom utils read.csv
#' @importFrom osfr osf_retrieve_node osf_ls_files osf_download
#'
#' @export
get_lib <- function(type = c("derivative", "nobaseline", "raw", "mediod",
                             "model"),
                    path = "system", node = "x7dpz", conflicts = "overwrite",
                    ...) {
  lp <- ifelse(path == "system",
               system.file("extdata", package = "OpenSpecy"),
               path)

  osf <- osf_retrieve_node(node) |>
    osf_ls_files(pattern = ".rds", n_max = Inf)

  message("Fetching Open Specy reference libraries from OSF ...")
    osf |> subset(grepl(
      paste0("(", paste(type, collapse = "|"), ").rds"),
      osf$name)) |>
      osf_download(path = lp, conflicts = conflicts, progress = TRUE, ...)

  message("Use 'load_lib()' to load the library")
}

#' @rdname manage_lib
#'
#' @export
load_lib <- function(type, path = "system") {
  lp <- ifelse(path == "system",
               system.file("extdata", package = "OpenSpecy"),
               path)

  chk <- .chkf(type, path = lp, condition = "stop")

  fp <- file.path(lp, paste0(type, ".rds"))
  rds <- readRDS(fp)

  return(rds)
}

#' @rdname manage_lib
#'
#' @export
rm_lib <- function(type = c("derivative", "nobaseline", "raw", "mediod",
                            "model"),
                   path = "system") {
  lp <- ifelse(path == "system",
               system.file("extdata", package = "OpenSpecy"),
               path)

  fp <- file.path(lp, paste0(type, ".rds"))
  file.remove(fp)

  invisible()
}

# Auxiliary function for library checks
.chkf <- function(type, path = "system", condition = "warning") {
  fn <- paste0(type, ".rds")

  lp <- ifelse(path == "system", system.file("extdata", package = "OpenSpecy"),
               path)

  chk <- file.path(lp, fn) |> file.exists()

  names(chk) <- type

  out <- paste(type[!chk], collapse = ", ")

  if (!all(chk))
    do.call(condition, list("Library missing or incomplete: ", out, "; ",
                            "use 'get_lib()' to download a current version",
                            call. =  ifelse(condition %in%
                                              c("message",
                                                "packageStartupMessage"),
                                            "", FALSE)))
  chk
}
