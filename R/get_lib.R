#' @title Download spectral libraries
#'
#' @description
#' Description
#'
#' @param which which
#' @param \ldots ...
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
#' @importFrom utils read.csv
#' @importFrom osfr osf_retrieve_node osf_ls_files osf_download
#' @export
get_lib <- function(which = c("ftir", "raman"), ...) {
  pkg <- system.file("extdata", package = "OpenSpecy")
  osf <- osf_retrieve_node("m38nq") %>%
    osf_ls_files(path = "data", pattern = ".csv", n_max = Inf)

  osf_download(osf, path = pkg, progress = TRUE)

  files <- list.files(pkg, full.names = TRUE) %>%
    lapply(read.csv)
  names(files) <- list.files(pkg) %>% ~gsub(".csv", "", .)

  file.remove(list.files(pkg, full.names = TRUE))

  for (f in 1:length(files)) {
    save(files, file = file.path(pkg, paste0(names(files)[f], ".RData")))
  }

  invisible()
}
