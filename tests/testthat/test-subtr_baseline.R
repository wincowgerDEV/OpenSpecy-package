data("raman_hdpe")

test_that("polynomial subtr_baseline() works as expected", {
  poly <- subtr_baseline(raman_hdpe, degree = 8) |> expect_silent()

  cor(poly$spectra[, "intensity"],
      subtr_baseline(raman_hdpe, degree = 1)$spectra[, "intensity"]) |> round(4) |>
    expect_equal(0.9929, ignore_attr = F)
  expect_s3_class(poly, "OpenSpecy")
  expect_true(check_OpenSpecy(poly))
  
  expect_equal(nrow(poly$spectra), nrow(raman_hdpe$spectra))
  expect_equal(poly$wavenumber, raman_hdpe$wavenumber)
  expect_equal(range(poly$spectra), c(0, 1))
})

test_that("manual subtr_baseline() works as expected", {
  subtr_baseline(raman_hdpe, type = "manual") |> expect_error()

  bl <- raman_hdpe
  bl$spectra[, "intensity"] <- bl$spectra[, "intensity"] / 2

  man <- subtr_baseline(raman_hdpe, type = "manual", baseline = bl) |>
    expect_silent()
  expect_true(check_OpenSpecy(man))

  cor(raman_hdpe$spectra[, "intensity"], man$spectra[, "intensity"]) |>
    expect_equal(1, ignore_attr = F)
  expect_equal(nrow(man$spectra), nrow(raman_hdpe$spectra))
  expect_equal(man$wavenumber, raman_hdpe$wavenumber)
  expect_equal(range(man$spectra), c(0, 1))
})

test_that("smodpoly subtr_baseline() works as expected", {
    smod <- subtr_baseline(x = raman_hdpe, type = "polynomial",full = F,
                           iterations = 10, refit_at_end = T,
                           remove_peaks = T,
                           peak_width_mult = 3, 
                           degree = 10, degree_part = 2) |> 
        expect_silent()
    
    # Check output structure and validity
    expect_s3_class(smod, "OpenSpecy")
    expect_true(check_OpenSpecy(smod))
    
    # Validate dimensions remain consistent
    expect_equal(nrow(smod$spectra), nrow(raman_hdpe$spectra))
    expect_equal(smod$wavenumber, raman_hdpe$wavenumber)
    
    # Check intensity range is normalized
    expect_equal(range(smod$spectra), c(0, 1))
    
    # Correlation between original and corrected spectra should be less than polynomial fitting
    poly <- subtr_baseline(raman_hdpe, degree = 8)
    cor_poly <- cor(poly$spectra[, "intensity"], smod$spectra[, "intensity"])
    expect_true(cor_poly > 0.95)

})

fill_peaks_fixture <- function() {
  values <- raman_hdpe$spectra[, "intensity"]
  spectra <- cbind(
    sample_b = values,
    sample_a = values * 0.8 + sin(raman_hdpe$wavenumber / 75) / 20
  )
  as_OpenSpecy(
    raman_hdpe$wavenumber,
    spectra,
    metadata = data.frame(source = c("b", "a")),
    attributes = list(
      intensity_unit = "absorbance",
      derivative_order = "0",
      baseline = "raw"
    )
  )
}

test_that("fill_peaks matches a canonical 4S reference calculation", {
  wavenumber <- seq(500, 1600, length.out = 12)
  spectra <- cbind(
    a = sin(wavenumber / 130) + (wavenumber - 900)^2 / 1e6,
    b = cos(wavenumber / 170) / 2 + seq_along(wavenumber) / 20
  )
  expected <- structure(c(
    -0.231465750716012, -0.673794263693740, -0.535661971932282,
     0.063067028111824,  0.754425061016937,  1.121347565776030,
     0.952505336184519,  0.341325906032740, -0.350134404301927,
    -0.715252015957970, -0.511624447506763,  0.194147922120500,
    -0.398798703979979, -0.364134192110769, -0.173873081279151,
     0.110561743185509,  0.396090186077281,  0.575149351492168,
     0.599452223198640,  0.458619117218035,  0.211969408773336,
    -0.069211977054000, -0.287844684562732, -0.367874571849971
  ), dim = c(12L, 2L), dimnames = list(NULL, c("a", "b")))
  reference <- subtr_baseline(
    as_OpenSpecy(wavenumber, spectra), type = "fill_peaks", lambda = 4,
    hwi = 2, it = 3, int = 4, make_rel = FALSE
  )
  expect_equal(reference$spectra, expected, tolerance = 1e-10)

  x <- fill_peaks_fixture()
  result <- subtr_baseline(
    x, type = "fill_peaks", lambda = 4, hwi = 5, it = 3, int = 25,
    make_rel = FALSE
  )

  expect_identical(result$wavenumber, x$wavenumber)
  expect_identical(result$metadata, x$metadata)
  expect_identical(colnames(result$spectra), colnames(x$spectra))
  expect_identical(attr(result, "intensity_unit"), "absorbance")
  expect_identical(attr(result, "derivative_order"), "0")
  expect_identical(attr(result, "baseline"), "raw")
  expect_true(check_OpenSpecy(result))
})

test_that("fill_peaks derives buckets and supports vector input", {
  x <- fill_peaks_fixture()
  derived_int <- min(
    nrow(x$spectra),
    max(3L, as.integer(round(nrow(x$spectra) / 10)))
  )
  expected <- subtr_baseline(
    x, type = "fill_peaks", lambda = 4, hwi = 5, it = 2,
    int = derived_int, make_rel = FALSE
  )

  result <- subtr_baseline(
    x, type = "fill_peaks", lambda = 4, hwi = 5, it = 2,
    make_rel = FALSE
  )
  expect_equal(result$spectra, expected$spectra, tolerance = 1e-12)

  y <- stats::setNames(x$spectra[, 1L], paste0("p", seq_len(nrow(x$spectra))))
  vector_result <- subtr_baseline(
    x$wavenumber, y = y, type = "fill_peaks", lambda = 4, hwi = 5,
    it = 2, int = derived_int, make_rel = FALSE
  )
  expect_equal(unname(vector_result), expected$spectra[, 1L],
               tolerance = 1e-12)
  expect_identical(names(vector_result), names(y))
})

test_that("fill_peaks base R banded smoothing matches a dense solve", {
  spectra <- rbind(
    seq(-1, 1, length.out = 40)^2 + sin(seq_len(40) / 3),
    cos(seq_len(40) / 5) + seq_len(40) / 100
  )
  second_difference <- diff(diag(ncol(spectra)), differences = 2)
  system <- diag(ncol(spectra)) +
    10^4 * crossprod(second_difference)
  expected <- t(solve(system, t(spectra)))
  actual <- OpenSpecy:::.fill_peaks_smooth(spectra, lambda = 4)

  expect_equal(unname(actual), unname(expected), tolerance = 1e-10)
  expect_identical(OpenSpecy:::.fill_peaks_smooth(spectra, lambda = 0),
                   spectra)
})

test_that("fill_peaks iterations fall back without overriding explicit it", {
  x <- fill_peaks_fixture()
  from_iterations <- subtr_baseline(
    x, type = "fill_peaks", lambda = 4, hwi = 5, iterations = 2,
    int = 25, make_rel = FALSE
  )
  explicit_two <- subtr_baseline(
    x, type = "fill_peaks", lambda = 4, hwi = 5, it = 2,
    int = 25, make_rel = FALSE
  )
  explicit_three <- subtr_baseline(
    x, type = "fill_peaks", lambda = 4, hwi = 5, iterations = 2, it = 3,
    int = 25, make_rel = FALSE
  )

  expect_equal(from_iterations$spectra, explicit_two$spectra,
               tolerance = 1e-12)
  expected_three <- subtr_baseline(
    x, type = "fill_peaks", lambda = 4, hwi = 5, it = 3,
    int = 25, make_rel = FALSE
  )
  expect_equal(explicit_three$spectra, expected_three$spectra,
               tolerance = 1e-12)
  expect_false(isTRUE(all.equal(explicit_two$spectra,
                                explicit_three$spectra)))
})

test_that("fill_peaks normalization and process_spec pass-through work", {
  x <- fill_peaks_fixture()
  corrected <- subtr_baseline(
    x, type = "fill_peaks", lambda = 4, hwi = 5, it = 2, int = 25,
    make_rel = FALSE
  )
  normalized <- subtr_baseline(
    x, type = "fill_peaks", lambda = 4, hwi = 5, it = 2, int = 25,
    make_rel = TRUE
  )
  expect_equal(normalized$spectra, make_rel(corrected$spectra),
               tolerance = 1e-12)

  processed <- process_spec(
    x,
    conform_spec = FALSE,
    smooth_intens = FALSE,
    subtr_baseline = TRUE,
    subtr_baseline_args = list(
      type = "fill_peaks", lambda = 4, hwi = 5, it = 2, int = 25
    ),
    make_rel = FALSE
  )
  expect_equal(processed$spectra, corrected$spectra, tolerance = 1e-12)
  expect_identical(processed$metadata, x$metadata)
  expect_identical(attr(processed, "baseline"), "nobaseline")
  expect_true(check_OpenSpecy(processed))
})

test_that("fill_peaks validates method-specific inputs", {
  x <- fill_peaks_fixture()
  common <- list(x = x, type = "fill_peaks", hwi = 5, it = 2,
                 int = 25, make_rel = FALSE)

  expect_error(do.call(subtr_baseline, utils::modifyList(common,
                                                         list(lambda = -1))),
               "lambda")
  expect_error(do.call(subtr_baseline, utils::modifyList(common,
                                                         list(hwi = 0))),
               "hwi")
  expect_error(do.call(subtr_baseline, utils::modifyList(common,
                                                         list(it = 1.5))),
               "it")
  expect_error(do.call(subtr_baseline, utils::modifyList(common,
                                                         list(int = 2))),
               "int")
  expect_error(do.call(subtr_baseline, utils::modifyList(
    common, list(int = nrow(x$spectra) + 1L)
  )), "int")

  nonfinite <- x
  nonfinite$spectra[1L, 1L] <- NA_real_
  expect_error(
    subtr_baseline(nonfinite, type = "fill_peaks", hwi = 5, it = 2,
                   int = 25, make_rel = FALSE),
    "finite"
  )

  expect_error(
    subtr_baseline(x, type = "not_a_method"),
    "arg"
  )
  expect_error(
    subtr_baseline(1:3, y = c(1, 2, 1), type = "fill_peaks",
                   make_rel = FALSE),
    "at least four"
  )
  shortest <- subtr_baseline(
    1:4, y = c(1, 2, 2, 1), type = "fill_peaks", lambda = 0,
    hwi = 1, it = 1, make_rel = FALSE
  )
  expect_length(shortest, 4L)
  expect_true(all(is.finite(shortest)))
  shortest_smoothed <- subtr_baseline(
    1:4, y = c(1, 2, 2, 1), type = "fill_peaks", lambda = 4,
    hwi = 1, it = 1, make_rel = FALSE
  )
  expect_length(shortest_smoothed, 4L)
  expect_true(all(is.finite(shortest_smoothed)))
  expect_error(
    subtr_baseline(1:4, y = c(1, 2, 2, 1), type = "fill_peaks",
                   lambda = 400, hwi = 1, it = 1, make_rel = FALSE),
    "too large"
  )
})


