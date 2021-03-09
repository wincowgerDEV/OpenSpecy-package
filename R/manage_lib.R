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
#' \code{check_lib()} checks to see if the Open Specy library already exists
#' on the users computer.
#' \code{get_lib()} will download the Open Specy Library from OSF
#' (\url{https://osf.io/x7dpz/}).
#' \code{load_lib()} will load the library into the global environment for use
#' with the Open Specy functions.
#'
#' @param which a character string specifying which library to use,
#' \code{"raman"} or \code{"ftir"}.
#' @param node the OSF node to be retrieved. Should be \code{"x7dpz"}.
#' @param location where to save or look for library files.
#' @param conflicts determines what happens when a file with the same name
#' exists at the specified destination. Can be one of the following (see
#' \code{\link[osfr]{osf_download}()} for details):
#' \itemize{
#'   \item{"error"}{ throw an error and abort the file transfer operation.}
#'   \item{"skip"}{ skip the conflicting file(s) and continue transferring the remaining files.}
#'   \item{"overwrite" (default)}{ replace the existing file with the transferred copy.}
#' }
#' @param condition determines if \code{check_lib()} should warn
#' (\code{"warning"}, the default) or throw and error (\code{"error"}).
#' @param \ldots further arguments passed to \code{\link[osfr]{osf_download}()}.
#'
#' @seealso
#' \code{\link{match_spectrum}()}
#'
#' @examples
#' \dontrun{
#' check_lib(which = c("ftir", "raman"))
#' get_lib(which = c("ftir", "raman"))
#'
#' spec_lib <- load_lib(which = c("ftir", "raman"))
#' }
#'
#' @importFrom magrittr %>%
#'
#' @export
check_lib <- function(which = c("ftir", "raman"),
                      location = system.file("extdata", package = "OpenSpecy"),
                      condition = "warning") {
  sapply(which, .chkf, location = location, condition = condition)

  invisible()
}

#' @rdname manage_lib
#'
#' @importFrom utils read.csv
#' @importFrom osfr osf_retrieve_node osf_ls_files osf_download
#'
#' @export
get_lib <- function(which = c("ftir", "raman"),
                    location = system.file("extdata", package = "OpenSpecy"),
                    node = "x7dpz", conflicts = "overwrite", ...) {
  osf <- osf_retrieve_node(node) %>%
    osf_ls_files(pattern = ".rds", n_max = Inf)

  cat("Fetching data from OSF ... \n\n")
  for (w in which) {
    osf %>% subset(grepl(paste0("^", w, "_.*"), osf$name)) %>%
      osf_download(path = location, conflicts = conflicts, progress = TRUE, ...)
  }
  cat("Done\n\n")

  message("Use 'load_lib()' to load the library")
}

#' @rdname manage_lib
#'
#' @export
load_lib <- function(which = c("ftir", "raman"),
                     location = system.file("extdata", package = "OpenSpecy")) {
  chk <- lapply(which, .chkf, location = location, condition = "stop")

  res <- lapply(chk, function(x) {
    fls <- file.path(location, paste0(x[[2L]], "_", names(x[[1L]]), ".rds"))

    rrds <- lapply(fls, readRDS)
    names(rrds) <- names(x[[1L]])

    rrds
  })

  names(res) <- which
  return(res)
}

# Auxiliary function for library checks
.chkf <- function(which, types = c("metadata", "peaks", "library"),
                  location = system.file("extdata", package = "OpenSpecy"),
                  condition = "warning") {
  fn <- paste0(which, "_", types, ".rds")

  chk <- file.path(location, fn) %>% file.exists()
  names(chk) <- types

  out <- switch (which,
                 "ftir" = "FTIR",
                 "raman" = "Raman"
  )

  if (!all(chk)) do.call(condition, list(out, " library missing or incomplete; ",
                                         "use 'get_lib()' to download a current version",
                                         call. =  ifelse(condition %in%
                                           c("message", "packageStartupMessage"),
                                           "", FALSE)))
  list(chk, which, out)
}
