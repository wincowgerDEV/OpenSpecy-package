map <- read_extdata("testdata_zipped.zip") |> read_any()

test_that("extdata files are present", {
    ed <- read_extdata()
    expect_true(any(grepl("\\.yml$", ed)))
    expect_true(any(grepl("\\.json$", ed)))
    expect_true(any(grepl("\\.rds$", ed)))
})