#' @importFrom utils packageVersion
if (getRversion() >= "2.15.1") {
  utils::globalVariables(c(
    ".", "..accuracy_columns", "..duplicate_key", ".library_order", ".N",
    ".object_order", ".row_id", ".rs.invokeShinyWindowExternal", "V1",
    "absolute_correlation", "action", "actual_name", "agreement", "artifact",
    "assessment_kind", "available", "check", "class_accuracy", "column",
    "correlation", "correct", "count", "dimension_units", "emissivity_model",
    "emissivity_physical_fraction", "emissivity_roughness",
    "emissivity_statistic", "emissivity_value",
    "estimated_material_temperature_k", "exact_material", "example_ids",
    "expected", "expected_class", "finding_count", "fit_max_cm1",
    "fit_min_cm1", "group_id", "initial_n", "intensity_unit",
    "intensity_units", "is_protected", "level", "library_id", "library_name",
    "match_identity",
    "match_val", "matched", "material", "material_class",
    "material_temperature_k", "material_type", "materials", "metric",
    "misidentified", "model", "n", "name", "object_id", "observed_n",
    "original", "path", "patterns", "physical_id", "pool", "populated_class",
    "predicted_class", "problem", "provenance", "query_id", "rate",
    "regex_materials", "remainder", "retained", "root", "sample_name",
    "schedule_order", "scope", "score", "selected", "shortfall", "size",
    "spectra", "spectrum_id", "spectrum_identity", "spectrum_type", "stage",
    "status", "stratum", "strongest", "technique", "temperature_source",
    "tie_break", "token", "valid_band_fraction", "value", "value_id",
    "value_index", "wavenumber", "weight", "x", "y"
  ))
}

.onAttach <- function(libname, pkgname) {
  if (!interactive()) return()

  packageStartupMessage("Running OpenSpecy ", packageVersion("OpenSpecy"))
  check_lib(condition = "packageStartupMessage")
}
