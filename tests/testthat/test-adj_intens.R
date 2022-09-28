data("raman_hdpe")

test_that("adj_intens() works as expected", {
  expect_silent(adj <- adj_intens(raman_hdpe))
  expect_identical(
    adj,
    adj_intens(raman_hdpe$wavenumber, raman_hdpe$intensity))
  expect_identical(
    adj,
    adj_intens(intensity ~ wavenumber, raman_hdpe))
  expect_equal(
    cor(raman_hdpe[2], adj_intens(raman_hdpe)[2]) %>% as.numeric(),
    1, ignore_attr = F)
  expect_s3_class(adj, "data.frame")
  expect_equal(names(adj), c("wavenumber", "intensity"))
  expect_equal(nrow(adj), nrow(raman_hdpe))
  expect_equal(adj[1], raman_hdpe[1])
  expect_equal(adj_intens(raman_hdpe, make_rel = T)[2] %>% range(),
               c(0, 1))
})
