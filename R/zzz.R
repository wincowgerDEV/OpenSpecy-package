#' @importFrom utils packageVersion
if (getRversion() >= "2.15.1") {
  utils::globalVariables(c(
    ".", ".N", ".row_id", ".rs.invokeShinyWindowExternal", "column",
    "correct", "level", "n", "problem", "value", "y"
  ))
}

.onAttach <- function(libname, pkgname) {
  if (!interactive()) return()

  packageStartupMessage("Running OpenSpecy ", packageVersion("OpenSpecy"))
  check_lib(condition = "packageStartupMessage")
}
