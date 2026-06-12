#' @importFrom utils packageVersion
if (getRversion() >= "2.15.1") {
  utils::globalVariables(c(
    ".", ".N", ".rs.invokeShinyWindowExternal", "column", "correct", "level",
    "n", "problem", "value"
  ))
}

.onAttach <- function(libname, pkgname) {
  if (!interactive()) return()

  packageStartupMessage("Running OpenSpecy ", packageVersion("OpenSpecy"))
  check_lib(condition = "packageStartupMessage")
}
