test_that("material percentage uncertainty implements the published equation", {
  expected <- 100 * abs(stats::qnorm(0.025)) * sqrt(0.5 * 0.5 / 100)
  expect_equal(material_percentage_uncertainty(100, 50), expected)
  expect_equal(
    material_percentage_uncertainty(100, c(0, 20, 100)),
    c(0, 100 * abs(stats::qnorm(0.025)) * sqrt(0.2 * 0.8 / 100), 0)
  )
  expect_lt(material_percentage_uncertainty(100, 50, confidence = 0.90),
            expected)
})

test_that("material percentage uncertainty vectorizes without ambiguous recycling", {
  expect_length(material_percentage_uncertainty(c(100, 200), 25), 2L)
  expect_length(material_percentage_uncertainty(100, c(25, 50), c(0.9, 0.95)),
                2L)
  expect_error(
    material_percentage_uncertainty(c(100, 200), c(10, 20, 30)),
    "shared common length"
  )
})

test_that("material percentage uncertainty rejects invalid inputs", {
  expect_error(material_percentage_uncertainty(0, 50), "positive whole")
  expect_error(material_percentage_uncertainty(10.5, 50), "positive whole")
  expect_error(material_percentage_uncertainty(100, -1), "between 0 and 100")
  expect_error(material_percentage_uncertainty(100, 101), "between 0 and 100")
  expect_error(material_percentage_uncertainty(100, 50, 1), "exclusive")
  expect_error(material_percentage_uncertainty(100, NA_real_), "finite numeric")
  expect_error(material_percentage_uncertainty(numeric(), 50), "length one")
})

test_that("particle uncertainty columns include clipped intervals and total RSD", {
  fields <- OpenSpecy:::.particle_uncertainty_columns(c(25, 75))
  expect_identical(fields$total_particle_count, c(100L, 100L))
  expect_equal(fields$percentage, c(25, 75))
  expect_equal(fields$total_concentration_rsd, c(0.1, 0.1))
  expect_true(all(fields$percentage_ci_lower >= 0))
  expect_true(all(fields$percentage_ci_upper <= 100))
  expect_named(
    OpenSpecy:::.particle_uncertainty_columns(numeric()),
    c("total_particle_count", "percentage", "confidence_level",
      "percentage_uncertainty", "percentage_ci_lower",
      "percentage_ci_upper", "total_concentration_rsd")
  )
})
