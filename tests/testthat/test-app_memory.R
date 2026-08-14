memory_test_object <- function(waves = 8L, spectra = 6L) {
  structure(list(
    wavenumber = seq_len(waves),
    spectra = matrix(
      seq_len(waves * spectra), nrow = waves, ncol = spectra
    ),
    metadata = data.frame(
      x = seq_len(spectra), y = rep(1L, spectra),
      row.names = paste0("s", seq_len(spectra))
    )
  ), class = "OpenSpecy")
}

test_that("available-memory overrides are deterministic and explicit", {
  .app_reset_memory_probe_cache()
  on.exit(.app_reset_memory_probe_cache(), add = TRUE)

  argument <- .app_available_memory(available_bytes = 4 * 1024^3)
  expect_true(argument$known)
  expect_equal(argument$bytes, 4 * 1024^3)
  expect_equal(argument$source, "argument")

  old <- options(OpenSpecy.available_memory_bytes = 3 * 1024^3)
  on.exit(options(old), add = TRUE)
  configured <- .app_available_memory()
  expect_true(configured$known)
  expect_equal(configured$bytes, 3 * 1024^3)
  expect_match(configured$source, "OpenSpecy.available_memory_bytes")
})

test_that("a failed live probe is cached rather than retried", {
  .app_reset_memory_probe_cache()
  on.exit(.app_reset_memory_probe_cache(), add = TRUE)

  calls <- 0L
  failing_probe <- function() {
    calls <<- calls + 1L
    stop("probe unavailable")
  }

  first <- .app_available_memory(probe = failing_probe)
  second <- .app_available_memory(probe = failing_probe)

  expect_false(first$known)
  expect_false(first$cached)
  expect_false(second$known)
  expect_true(second$cached)
  expect_equal(calls, 1L)
  expect_match(second$reason, "probe unavailable")
})

test_that("successful live probes refresh available memory", {
  .app_reset_memory_probe_cache()
  on.exit(.app_reset_memory_probe_cache(), add = TRUE)

  calls <- 0L
  changing_probe <- function() {
    calls <<- calls + 1L
    list(bytes = calls * 1024^3, source = "changing test probe")
  }

  first <- .app_available_memory(probe = changing_probe)
  second <- .app_available_memory(probe = changing_probe)

  expect_true(first$known)
  expect_true(second$known)
  expect_false(first$cached)
  expect_false(second$cached)
  expect_equal(first$bytes, 1024^3)
  expect_equal(second$bytes, 2 * 1024^3)
  expect_equal(calls, 2L)
})

test_that("preflight accounts for blockwise top-N and avoided full matrices", {
  object <- memory_test_object(waves = 8L, spectra = 150L)
  result <- .app_memory_preflight(
    object,
    library_size = 20L,
    top_n = 10L,
    block_size = 100L,
    available_bytes = 64 * 1024^3
  )

  expect_true(result$safe)
  expect_equal(result$status, "safe")
  expect_equal(result$block_size, 100L)
  expect_equal(result$effective_block_size, 100L)
  expect_equal(result$block_score_bytes, 8 * 20 * 100)
  expect_equal(result$compact_top_n_bytes, 32 * 150 * 10)
  expect_equal(result$avoided_full_matrix_bytes, 8 * 20 * 150)
  expect_equal(
    unname(result$phase_peak_bytes - result$phase_workspace_bytes),
    rep(result$estimated_loaded_bytes, 3L)
  )
  expect_equal(result$estimated_loaded_bytes,
               result$resident_bytes + result$library_resident_bytes)
  expect_gt(result$query_block_bytes, 0)
  expect_match(result$message, "reference-library estimate")
  expect_match(result$message, "within the measured RAM")
})

test_that("invalid block sizes use the same 100-spectrum fallback", {
  object <- memory_test_object()

  missing_block <- .app_memory_preflight(
    object, library_size = 100L, block_size = NA_integer_,
    available_bytes = 1024^3
  )
  null_block <- .app_memory_preflight(
    object, library_size = 100L, block_size = NULL,
    available_bytes = 1024^3
  )

  expect_equal(missing_block$block_size, 100L)
  expect_equal(null_block$block_size, 100L)
  expect_equal(missing_block$block_score_bytes, 8 * 100 * 6)
})

test_that("unsafe estimates provide phase-specific remedies", {
  object <- memory_test_object(waves = 100L, spectra = 80L)
  result <- .app_memory_preflight(
    object,
    pca_components = 10L,
    clusters = 10L,
    available_bytes = 1,
    reserve_fraction = 0.25
  )

  expect_false(result$safe)
  expect_equal(result$status, "unsafe")
  expect_equal(result$peak_phase, "pca_clustering")
  expect_match(result$message, "Lower PCA components")
  expect_match(result$message, "Crop or split")
})

test_that("unknown RAM is never reported as safe", {
  .app_reset_memory_probe_cache()
  on.exit(.app_reset_memory_probe_cache(), add = TRUE)

  result <- .app_memory_preflight(
    memory_test_object(),
    library_size = 10L,
    probe = function() stop("not exposed")
  )

  expect_true(is.na(result$safe))
  expect_equal(result$status, "unknown")
  expect_match(result$message, "not exposed")
  expect_match(result$message, "not being treated as safe")
  expect_match(result$message, "10 GB upload ceiling")
})

test_that("clustering estimates track effective PCA and k-means sizes", {
  object <- memory_test_object(waves = 8L, spectra = 6L)
  result <- .app_memory_preflight(
    object,
    pca_components = 99L,
    clusters = 99L,
    available_bytes = 1024^3
  )

  expect_equal(result$effective_pca_components, 6L)
  expect_equal(result$effective_clusters, 6L)
  expect_gt(result$phase_workspace_bytes[["pca_clustering"]],
            result$spectra_bytes)
})
