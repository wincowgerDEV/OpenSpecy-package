context("Human readable timestamps")

test_that("Make sure timestamps really differ", {
  t1 <- human_timestamp()
  Sys.sleep(1)
  t2 <- human_timestamp()
  expect_false(isTRUE(all.equal(t1, t2)))
})
