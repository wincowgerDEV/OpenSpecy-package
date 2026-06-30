test_that("detection limit helpers match workflow formulas", {
  count <- 11
  expect_equal(batch_detection_limit(count),
               as.integer(count + 3 + 4.65 * sqrt(count)))

  blanks <- data.frame(
    sample_id = c("b1", "b2", "b3", "b4"),
    area_bins = "(0,212]",
    count = c(0, 0, 0, 11)
  )
  mda <- minimum_detectable_amount(blanks, group_cols = "area_bins")
  expected <- mean(blanks$count) + 3 +
    3.29 * sd(blanks$count) * sqrt(1 + 1 / 4)
  expect_equal(mda$MDA, as.integer(expected))
  expect_equal(mda$method, "MDA")

  one_blank <- minimum_detectable_amount(blanks[1, ], group_cols = "area_bins")
  expect_equal(one_blank$method, "BDL")
})

test_that("recovery and crowding summaries use user supplied columns", {
  spikes <- data.frame(
    material = c("a", "a", "b"),
    observed = c(8, 9, 4),
    expected = c(10, 10, 5),
    pre = c(1, 0, 0)
  )
  rec <- recovery_rate(spikes, observed_col = "observed",
                       expected_col = "expected",
                       pre_recovered_col = "pre",
                       group_cols = "material")
  expect_equal(rec$expected_spiked, c(20, 5))
  expect_true(all(rec$mean_recovery > 0))

  particles <- data.frame(
    sample_id = "s1",
    material = c("a", "a", "b"),
    area_um2 = c(100, 400, 900),
    min_length_um = c(10, 20, 30)
  )
  crowd <- crowd_lookup(particles, material_col = "material",
                        size_threshold = 50, seed = 1)
  expect_equal(nrow(crowd), 2)
  expect_true(all(crowd$potential_particle_ratio == 1))
})
