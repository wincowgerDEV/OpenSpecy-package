
test_that("Make sure run_app doesn't produce error", {
    #expect_no_error(run_app())
    expect_error(run_app(path = error))
})
