#' @importFrom magrittr %>%
#' @importFrom utils packageVersion
.onAttach <- function(libname, pkgname) {
  if (!interactive()) return()

  ver = packageVersion("OpenSpecy")
  irf <- c("ftir_metadata.RData", "ftir_peaks.RData", "ftir_library.RData.RData")
  rmf <- c("raman_metadata.RData", "raman_peaks.RData", "raman_library.RData")

  irb <- system.file("extdata", package = "OpenSpecy") %>%
    file.path(irf) %>% file.exists()
  rmb <- system.file("extdata", package = "OpenSpecy") %>%
    file.path(rmf) %>% file.exists()

  packageStartupMessage("Running OpenSpecy ", ver)

  if (!all(irb)) packageStartupMessage("FTIR library missing or incomplete; ",
                                      "use 'get_lib()' to download a current version")
  if (!all(rmb)) packageStartupMessage("Raman library missing or incomplete; ",
                                       "use 'get_lib()' to download a current version")
}
