map <- read_extdata("CA_tiny_map.zip") |> read_any()

binary_matrix <- matrix(c(TRUE, TRUE, FALSE,  
                          TRUE, TRUE, FALSE,
                          FALSE, FALSE, FALSE,
                          FALSE, FALSE, FALSE,
                          FALSE, TRUE, FALSE, 
                          FALSE, TRUE, TRUE,
                          FALSE, TRUE, FALSE), ncol = 3, byrow = T)

test_that("check that particles are identified", {
    ed <- read_extdata()
    expect_true(any(grepl("\\.yml$", ed)))
    expect_true(any(grepl("\\.json$", ed)))
    expect_true(any(grepl("\\.rds$", ed)))
})


