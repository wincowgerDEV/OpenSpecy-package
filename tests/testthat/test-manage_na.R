

test_that("manage_na works as expected", {
    manage_na(c(NA, -1, NA, 1, 10)) |> 
        expect_identical(c(TRUE, FALSE, FALSE, FALSE, FALSE))
    manage_na(c(NA, -1, NA, 1, 10), lead_tail_only = FALSE) |> 
        expect_identical(c(TRUE, FALSE, TRUE, FALSE, FALSE))
    manage_na(c(NA, 0, NA, 1, 10), lead_tail_only = FALSE, ig_zero = TRUE) |>
        expect_identical(c(TRUE, TRUE, TRUE, FALSE, FALSE))
    
    data(raman_hdpe)
    raman_hdpe$spectra[[1]][1:10] <- NA
    ignore <- manage_na(raman_hdpe, fun = make_rel) 
    ignore$spectra[[1]] |>
        is.na() |>
        sum() |>
        expect_equal(10)
    remove <- manage_na(raman_hdpe, type = "remove") 
    remove$spectra[[1]] |>
        is.na() |>
        sum() |>
        expect_equal(0)
})


test_that("manage_na works as expected", {
    data(raman_hdpe)
    raman_hdpe$spectra[[1]][3:nrow(raman_hdpe$spectra)] <- NA
    manage_na(raman_hdpe, fun = make_rel) |> expect_silent()
    raman_hdpe$spectra[[1]][1:nrow(raman_hdpe$spectra)] <- NA
    manage_na(raman_hdpe, fun = make_rel) |> expect_error()
})
