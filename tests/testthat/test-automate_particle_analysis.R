test_that("automate_particle_analysis() returns details and summaries", {
  wn <- seq(750, 1800, length.out = 40)
  pe <- sin(wn / 120) + 1
  mineral <- cos(wn / 130) + 1
  lib <- as_OpenSpecy(
    wn,
    spectra = cbind(pe = pe, mineral = mineral),
    metadata = data.frame(
      x = 0:1,
      y = 0,
      sample_name = c("pe", "mineral"),
      material_class = c("poly(ethylene)", "mineral")
    )
  )
  map <- as_OpenSpecy(
    wn,
    spectra = cbind(pe + 0.01, pe + 0.02, mineral + 0.01, rep(0, length(wn))),
    metadata = data.frame(x = c(0, 1, 0, 1), y = c(0, 0, 1, 1))
  )

  res <- automate_particle_analysis(
    map,
    lib,
    sn_threshold_min = 0.001,
    area_threshold = 0,
    outputs = c("details", "summary"),
    process_args = list(smooth_intens = FALSE, make_rel = TRUE)
  )

  expect_type(res, "list")
  expect_s3_class(res$details, "data.table")
  expect_s3_class(res$summary, "data.table")
  expect_gt(nrow(res$details), 0)
})

test_that("automate_particle_analysis() rejects removed legacy arguments", {
  wn <- 1:5
  os <- as_OpenSpecy(wn, spectra = matrix(seq_len(10), nrow = 5))
  expect_error(
    automate_particle_analysis(os, os, adj_map_baseline = TRUE),
    "Removed automate_particle_analysis"
  )
})
