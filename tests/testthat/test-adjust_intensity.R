test_that("adjust_intensity() works as expected", {
  expect_silent(adj <- adjust_intensity(raman_hdpe))
  expect_s3_class(adj, "data.frame")
  expect_equal(names(adj), c("wavenumber", "intensity"))
  expect_equal(nrow(adj), nrow(raman_hdpe))
  expect_equal(adj[1], raman_hdpe[1])
  expect_equal(range(adj[2]), c(0, 1))
})
