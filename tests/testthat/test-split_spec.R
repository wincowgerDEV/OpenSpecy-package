data("test_lib")
data("raman_hdpe")

test_that("split_spec() handles inputs correctly", {
    listed <- list(test_lib, raman_hdpe)
    test <- split_spec(listed)
    test2 <- split_spec(list(test_lib))
    test3 <- split_spec(list(raman_hdpe))
    expect_equal(length(test), 58)
    expect_equal(length(test2), 57)
    expect_equal(length(test3), 1)
})