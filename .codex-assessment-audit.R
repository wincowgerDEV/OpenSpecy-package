path <- "../reference-library-build-2.0.0/releases/2c1b3ce60210/assessments.rds"
x <- readRDS(path)

walk <- function(value, prefix = "assessments") {
  if (inherits(value, c("data.frame", "data.table"))) {
    n <- nrow(value)
    missing <- if (n) vapply(value, function(column) mean(is.na(column)),
                             numeric(1L)) else numeric()
    cat("\n", prefix, ": ", n, " x ", ncol(value), "\n", sep = "")
    if (length(missing)) {
      print(data.frame(column = names(missing), missing = missing,
                       row.names = NULL))
    }
    return(invisible(NULL))
  }
  if (!is.list(value)) return(invisible(NULL))
  for (name in names(value)) walk(value[[name]], paste(prefix, name, sep = "$"))
}

walk(x)
cat("\n--- evidence ---\n")
walk(attr(x, "evidence", exact = TRUE), "evidence")
cat("\n--- upstream ---\n")
walk(attr(x, "upstream_assessments", exact = TRUE), "upstream")

cat("\n--- quality keys ---\n")
upstream <- attr(x, "upstream_assessments", exact = TRUE)
print(unique(upstream$quality_control[, c("artifact", "check", "action")]))
cat("\n--- model test keys ---\n")
evidence <- attr(x, "evidence", exact = TRUE)
print(unique(evidence$model_tests[, c(
  "artifact", "model", "source", "technique"
)]))

cat("\n--- rebuilt compact review ---\n")
devtools::load_all(export_all = FALSE, quiet = TRUE)
components <- c(upstream, evidence)
components$library_identification <-
  OpenSpecy:::.lib_identification_summary(evidence$library_tests)
components$library_confusion <-
  OpenSpecy:::.lib_confusion_table(evidence$library_tests)
components$model_identification <-
  OpenSpecy:::.lib_identification_summary(evidence$model_tests)
components$model_confusion <-
  OpenSpecy:::.lib_confusion_table(evidence$model_tests)
flags <- unique(upstream$quality_control[, .(
  artifact, technique = spectrum_type, spectrum_id, error_mode = check
)])
components$model_assessment_correlations <-
  OpenSpecy:::.lib_model_assessment_correlations(evidence$model_tests, flags)
review <- OpenSpecy:::.lib_assessment_review(components)
walk(review, "compact")
tables <- unlist(lapply(review, function(section) {
  lapply(section, function(tab) {
    missing <- if (nrow(tab)) vapply(tab, function(column) mean(is.na(column)),
                                    numeric(1L)) else numeric()
    c(rows = nrow(tab), columns = ncol(tab),
      max_missing = if (length(missing)) max(missing) else 0)
  })
}), recursive = FALSE)
print(tables)
