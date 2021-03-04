#' @rdname manage_lib
#'
#' @title Manage spectral libraries
#'
#' @description
#' Description
#'
#' @param which which
#'
#' @seealso
#' seealso
#'
#' @examples
#' \dontrun{
#' get_lib()
#' }
#'
#' @importFrom magrittr %>%
#'
#' @export
check_lib <- function(which = c("ftir", "raman")) {
  types <- c("metadata", "peaks", "library")

  for (w in which) {
    n <- paste0(w, "_", types, ".RData")

    chk <- system.file("extdata", package = "OpenSpecy") %>%
      file.path(n) %>% file.exists()

    wout <- switch (w,
      "ftir" = "FTIR",
      "raman" = "Raman"
    )

    if (!all(chk)) packageStartupMessage(wout, " library missing or incomplete; ",
                                         "use 'get_lib()' to download a current version")
  }
}

#' @rdname manage_lib
#'
#' @param conflicts determines what happens when a file with the same name
#' exists at the specified destination. Can be one of the following (see
#' \code{\link[osfr]{osf_download}()} for details):
#' * "error": throw an error and abort the file transfer operation.
#' * "skip": skip the conflicting file(s) and continue transferring the remaining files.
#' * "overwrite" (the default): replace the existing file with the transferred copy.
#' @param \ldots arguments passed to \code{\link[osfr]{osf_download}()}
#'
#' @importFrom utils read.csv
#' @importFrom rlang .data
#' @importFrom dplyr filter
#' @importFrom osfr osf_retrieve_node osf_ls_files osf_download
#'
#' @export
get_lib <- function(which = c("ftir", "raman"), conflicts = "overwrite", ...) {
  pkg <- system.file("extdata", package = "OpenSpecy")
  osf <- osf_retrieve_node("g3axs") %>%
    osf_ls_files(pattern = ".RData", n_max = Inf)

  message("Fetching data from OSF ... \n")
  for (w in which) {
    osf %>% dplyr::filter(grepl(paste0(w, "*"), .data$name)) %>%
      osf_download(path = pkg, conflicts = conflicts, progress = TRUE, ...)
  }

  message("Done")
}

#' @rdname manage_lib
#'
#' @export
load_lib <- function(which = c("ftir", "raman")) {

}
