#' @importFrom utils packageVersion
.onAttach <- function(libname, pkgname) {
  if (!interactive()) return()

  packageStartupMessage("Running OpenSpecy ", packageVersion("OpenSpecy"))
  check_lib(condition = "packageStartupMessage")
}
