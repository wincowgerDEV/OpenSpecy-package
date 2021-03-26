#' @rdname manage_lib
#'
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
#'
#' @param which a character string specifying which library to use,
#' \code{"raman"} or \code{"ftir"}.
#' @param types library types to check/retrieve; defaults to
#' \code{c("metadata", "library", "peaks")}.
#' @param node the OSF node to be retrieved; should be \code{"x7dpz"}.
#' @param path where to save or look for local library files.
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
#' \code{load_lib()} returns a list object containing the respective spectral
#' reference library.
#'
#' @examples
#' \dontrun{
#' check_lib(which = c("ftir", "raman"))
#' get_lib(which = c("ftir", "raman"))
#'
#' spec_lib <- load_lib(which = c("ftir", "raman"))
#' }
#'
#' @author
#' Zacharias Steinmetz
#'
#' @seealso
#' \code{\link{match_spec}()}
#'
#' @references
#' Cowger W, Gray A, Christiansen SH, Christiansen SH, Christiansen SH,
#' De Frond H, Deshpande AD, Hemabessiere L, Lee E, Mill L, et al. (2020).
#' “Critical Review of Processing and Classification Techniques for Images and
#' Spectra in Microplastic Research.” \emph{Applied Spectroscopy},
#' \strong{74}(9), 989–1010. \doi{10.1177/0003702820929064}.
#'
#' Cowger, W (2021). “Library data.” \emph{OSF}. \doi{10.17605/OSF.IO/X7DPZ}.
#'
#' @importFrom magrittr %>%
#'
#' @export
check_lib <- function(which = c("ftir", "raman"),
                      types = c("metadata", "library", "peaks"),
                      path = system.file("extdata", package = "OpenSpecy"),
                      condition = "warning") {
  sapply(which, .chkf, types = types, path = path, condition = condition)

  invisible()
}

#' @rdname manage_lib
#'
#' @importFrom utils read.csv
#' @importFrom osfr osf_retrieve_node osf_ls_files osf_download
#'
#' @export
get_lib <- function(which = c("ftir", "raman"),
                    types = c("metadata", "library", "peaks"),
                    path = system.file("extdata", package = "OpenSpecy"),
                    node = "x7dpz", conflicts = "overwrite", ...) {
  osf <- osf_retrieve_node(node) %>%
    osf_ls_files(pattern = ".rds", n_max = Inf)

  message("Fetching Open Specy reference libraries from OSF ...")
  for (w in which) {
    osf %>% subset(grepl(
      paste0("^", w, "_(", paste(types, collapse = "|"), ").rds"),
      osf$name)) %>%
      osf_download(path = path, conflicts = conflicts, progress = TRUE, ...)
  }

  message("Use 'load_lib()' to load the library")
}

#' @rdname manage_lib
#'
#' @export
load_lib <- function(which = c("ftir", "raman"),
                     types = c("metadata", "library", "peaks"),
                     path = system.file("extdata", package = "OpenSpecy")) {
  chk <- lapply(which, .chkf, types = types, path = path, condition = "stop")

  res <- lapply(chk, function(x) {
    fls <- file.path(path, paste0(x[[2L]], "_", names(x[[1L]]), ".rds"))

    rrds <- lapply(fls, readRDS)
    names(rrds) <- names(x[[1L]])

    rrds
  })

  names(res) <- which
  return(res)
}

# Auxiliary function for library checks
.chkf <- function(which, types = c("metadata", "library", "peaks"),
                  path = system.file("extdata", package = "OpenSpecy"),
                  condition = "warning") {
  fn <- paste0(which, "_", types, ".rds")

  chk <- file.path(path, fn) %>% file.exists()
  names(chk) <- types

  out <- switch (which,
                 "ftir" = "FTIR",
                 "raman" = "Raman"
  )

  if (!all(chk)) do.call(condition, list(out, " library missing or incomplete; ",
                                         "use 'get_lib()' to download a current version",
                                         call. =  ifelse(condition %in%
                                                           c("message",
                                                             "packageStartupMessage"),
                                                         "", FALSE)))
  list(chk, which, out)
}
