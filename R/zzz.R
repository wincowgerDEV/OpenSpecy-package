#' @importFrom magrittr %>%
#' @importFrom utils packageVersion
.onAttach <- function(libname, pkgname) {
  if (!interactive()) return()

  ver = packageVersion("OpenSpecy")
  irf <- c("FTIRLibrary.RData", "FTIRLibraryMetadata.RData", "FTIRLibraryPeak.RData")
  rmf <- c("RamanLibrary.RData", "RamanLibraryMetadata.RData", "RamanLibraryPeak.RData")

  irb <- system.file("extdata", package = "OpenSpecy") %>%
    file.path(irf) %>% file.exists()
  rmb <- system.file("extdata", package = "OpenSpecy") %>%
    file.path(rmf) %>% file.exists()

  packageStartupMessage("Running OpenSpecy ", ver)

  if (!all(irb)) packageStartupMessage("No FTIR library found; ",
                                      "use 'get_lib()' to download a current version")
  if (!all(rmb)) packageStartupMessage("No Raman library found; ",
                                       "use 'get_lib()' to download a current version")
}
