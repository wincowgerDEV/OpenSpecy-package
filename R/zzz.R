#' @importFrom utils packageVersion
if (getRversion() >= "2.15.1") {
  utils::globalVariables(c(
    ".", "..accuracy_columns", "..duplicate_key", "..flag_required",
    "..keep", "..test_required", ".library_order", ".N", ".source_order",
    ".object_order", ".row_id", ".rs.invokeShinyWindowExternal", "V1",
    "absolute_correlation", "action", "active_degree", "actual_name",
    "agreement", "artifact",
    "assessment", "assessment_kind", "available", "check", "class_accuracy",
    "class_count", "class_left", "class_n_before", "class_right", "column",
    "component_id", "correlation", "correlation_view", "correct", "count",
    "decision_round", "dimension_units",
    "evidence_libraries", "emissivity_model",
    "emissivity_physical_fraction", "emissivity_roughness",
    "emissivity_statistic", "emissivity_value",
    "estimated_material_temperature_k", "exact_material", "example_ids",
    "error_mode", "error_spectra", "evaluated_classes", "expected",
    "expected_class",
    "expected_class_fraction", "expected_class_pct", "finding_count",
    "fit_max_cm1", "from",
    "fit_min_cm1", "group_id", "has_error", "initial_n", "intensity_unit",
    "intensity_units", "is_protected", "level", "library_id", "library_name",
    "match_identity",
    "macro_class_accuracy", "macro_class_accuracy_pct", "match_val", "matched",
    "matched_class", "matched_id",
    "matched_evidence_libraries", "matched_library",
    "material", "material_class", "material_form", "common_use",
    "material_temperature_k", "material_type", "materials", "metric",
    "misidentified", "model", "N", "n", "name", "object_id", "observed_n",
    "original", "path", "patterns", "phase", "physical_id", "pool",
    "populated_class", "prior_class",
    "no_error_spectra", "overall_accuracy", "overall_accuracy_pct",
    "predicted_class", "problem", "provenance", "quarantine_object",
    "quarantine_status", "query_id", "rate",
    "rate_new", "rate_old", "reason", "regex_materials", "remainder",
    "resolved", "retained", "review_status", "root", "sample_name",
    "schedule_order", "scope", "score", "selected", "shortfall", "size",
    "spectra", "spectrum_id", "spectrum_identity", "spectrum_type", "stage",
    "status", "stratum", "strongest", "technique", "temperature_source",
    "threshold", "tie_break", "to", "token", "valid_band_fraction", "value",
    "value_id",
    "value_index", "wavenumber", "weight", "with_error_accuracy_pct",
    "without_error_accuracy_pct", "accuracy_difference_pct", "x", "y"
  ))
}

.onAttach <- function(libname, pkgname) {
  if (!interactive()) return()

  packageStartupMessage("Running OpenSpecy ", packageVersion("OpenSpecy"))
  check_lib(condition = "packageStartupMessage")
}
