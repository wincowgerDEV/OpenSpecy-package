#' @rdname manage_lib
#'
#' @title Manage spectral libraries
#'
#' @description
#' These functions will import the spectral libraries from Open Specy if they were not already downloaded. 
#' The CRAN does not allow for deployment of large datasets so this was a workaround that we are using to make sure everyone can easilly get Open Specy functionality running on their desktop.
#'
#' @details
#' \code{check_lib()} Checks to see if the Open Specy library already exists on the users computer.
#' \code{get_lib()} will download the Open Specy Library from OSF if it does not exist on the users computer. 
#' \code{load_lib()} will load the library into the global environment for use with the Open Specy functions. 
#'
#' @param which Which spectral library to check, raman or  ftir?
#' @param node node
#'
#' @seealso
#' \code{match_spectrum()}
#'
#' @examples
#' \dontrun{
#' check_lib(which = c("ftir", "raman"))
#' get_lib(which = c("ftir", "raman"))
#'
#' spec_lib <- load_lib(which = c("ftir", "raman"))
#' }
#'
#' @param condition condition
#'
#' @importFrom magrittr %>%
#'
#' @export
check_lib <- function(which = c("ftir", "raman"), condition = "warning") {
  sapply(which, .chkf, condition = condition)

  invisible()
}

#' @rdname manage_lib
#'
#' @param conflicts determines what happens when a file with the same name
#' exists at the specified destination. Can be one of the following (see
#' \code{\link[osfr]{osf_download}()} for details):
#' \itemize{
##'  \item{"error"}{throw an error and abort the file transfer operation.}
##'  \item{"skip"}{skip the conflicting file(s) and continue transferring the remaining files.}
##'  \item{"overwrite" (default)}{replace the existing file with the transferred copy.}
##' }
#' @param \ldots arguments passed to \code{\link[osfr]{osf_download}()}
#'
#' @importFrom utils read.csv
#' @importFrom osfr osf_retrieve_node osf_ls_files osf_download
#'
#' @export
get_lib <- function(which = c("ftir", "raman"), node = "x7dpz",
                    conflicts = "overwrite", ...) {
  pkg <- system.file("extdata", package = "OpenSpecy")
  osf <- osf_retrieve_node(node) %>%
    osf_ls_files(pattern = ".rds", n_max = Inf)

  cat("Fetching data from OSF ... \n\n")
  for (w in which) {
    osf %>% subset(grepl(paste0(w, "*"), osf$name)) %>%
      osf_download(path = pkg, conflicts = conflicts, progress = TRUE, ...)
  }
  cat("Done\n\n")

  message("Use 'load_lib()' to load the library")
}

#' @rdname manage_lib
#'
#' @export
load_lib <- function(which = c("ftir", "raman")) {
  chk <- lapply(which, .chkf, condition = "stop")
  pkg <- system.file("extdata", package = "OpenSpecy")

  res <- lapply(chk, function(x) {
    fls <- file.path(pkg, paste0(x[[2L]], "_", names(x[[1L]]), ".rds"))

    rrds <- lapply(fls, readRDS)
    names(rrds) <- names(x[[1L]])

    rrds
  })

  names(res) <- which
  return(res)
}

# Auxiliary function for library checks
.chkf <- function(x, types = c("metadata", "peaks", "library"),
                  condition = "warning") {
  fn <- paste0(x, "_", types, ".rds")

  chk <- system.file("extdata", package = "OpenSpecy") %>%
    file.path(fn) %>% file.exists()
  names(chk) <- types

  xout <- switch (x,
                  "ftir" = "FTIR",
                  "raman" = "Raman"
  )

  if (!all(chk)) do.call(condition, list(xout, " library missing or incomplete; ",
                                         "use 'get_lib()' to download a current version",
                                         call. =  ifelse(condition %in%
                                           c("message", "packageStartupMessage"),
                                           "", FALSE)))
  list(chk, x, xout)
}
