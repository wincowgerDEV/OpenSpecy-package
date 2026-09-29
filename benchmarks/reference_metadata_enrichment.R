# Representative kernel benchmark for official reference metadata enrichment.
# Run from the package root with:
# Rscript benchmarks/reference_metadata_enrichment.R

suppressPackageStartupMessages(devtools::load_all(".", quiet = TRUE))

benchmark_reference_metadata_enrichment <- function(
    rows = 25000L, columns = 50L, patterns = 75L, seed = 20260929L) {
  stopifnot(rows > 0L, columns >= 2L, patterns > 0L)
  set.seed(seed)
  vocabulary <- c("unknown", "sample", "clear", "aged", "reference")
  metadata <- data.table::as.data.table(replicate(
    columns - 1L,
    sample(vocabulary, rows, replace = TRUE),
    simplify = FALSE
  ))
  data.table::setnames(metadata, paste0("field_", seq_len(columns - 1L)))
  metadata[, material_form := NA_character_]
  metadata[seq.int(1L, rows, by = 7L), field_1 := paste(field_1, "paint")]
  metadata[seq.int(2L, rows, by = 11L), field_2 := paste(field_2, "fiber")]
  metadata[seq.int(3L, rows, by = 13L), material_form := "rubber"]

  reference <- data.table::fread(
    file.path("workflows", "data", "material_form_regex.csv")
  )
  if (patterns > nrow(reference)) {
    extra_count <- patterns - nrow(reference)
    extra <- data.table::data.table(
      pattern = sprintf(
        "(^|[^[:alnum:]])benchmark_token_%03d([^[:alnum:]]|$)",
        seq_len(extra_count)
      ),
      material_form = rep(reference$material_form, length.out = extra_count)
    )
    reference <- data.table::rbindlist(list(reference, extra))
  } else {
    reference <- reference[seq_len(patterns)]
  }

  invisible(gc(reset = TRUE))
  timing <- system.time({
    result <- OpenSpecy:::.lib_enrich_material_form(metadata, reference)
  })
  memory <- gc()
  data.table::data.table(
    rows = rows,
    columns = columns,
    patterns = patterns,
    elapsed_seconds = unname(timing[["elapsed"]]),
    gc_peak_mib = sum(memory[, 6L]),
    populated = result$summary[metric == "populated", value],
    conflicting = result$summary[metric == "conflicting", value]
  )
}

if (!interactive()) print(benchmark_reference_metadata_enrichment())
