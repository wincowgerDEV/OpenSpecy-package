

test_that("manage_na works as expected 1", {
    manage_na(c(NA, -1, NA, 1, 10)) |> 
        expect_identical(c(TRUE, FALSE, FALSE, FALSE, FALSE))
    manage_na(c(NA, -1, NA, 1, 10), lead_tail_only = FALSE) |> 
        expect_identical(c(TRUE, FALSE, TRUE, FALSE, FALSE))
    manage_na(c(NA, 0, NA, 1, 10), lead_tail_only = FALSE, ig = c(NA, 0)) |>
        expect_identical(c(TRUE, TRUE, TRUE, FALSE, FALSE))
    
    data(raman_hdpe)
    raman_hdpe$spectra[1:10, 1] <- NA
    ignore <- manage_na(raman_hdpe, fun = make_rel) 
    ignore$spectra[, 1] |>
        is.na() |>
        sum() |>
        expect_equal(10)
    remove <- manage_na(raman_hdpe, type = "remove") 
    remove$spectra[, 1] |>
        is.na() |>
        sum() |>
        expect_equal(0)
})


test_that("manage_na works with zeros", {
    data(raman_hdpe)
    raman_hdpe$spectra[1:10, 1] <- 0
    remove <- manage_na(raman_hdpe, ig = c(NA, 0), type = "remove") 
    expect_equal(nrow(remove$spectra), nrow(raman_hdpe$spectra) - 10)

    raman_hdpe$spectra[1:10, 1] <- NA
    keep_na <- manage_na(raman_hdpe, ig = 0, type = "remove")
    expect_equal(nrow(keep_na$spectra), nrow(raman_hdpe$spectra))
})


test_that("manage_na works as expected", {
    data(raman_hdpe)
    raman_hdpe$spectra[3:nrow(raman_hdpe$spectra), 1] <- NA
    manage_na(raman_hdpe, fun = make_rel) |> expect_silent()
    raman_hdpe$spectra[1:nrow(raman_hdpe$spectra), 1] <- NA
    manage_na(raman_hdpe, fun = make_rel) |> expect_error()
})

test_that("manage_na processes different valid ranges and keeps attributes", {
    data(raman_hdpe)
    two <- cbind(raman_hdpe$spectra[, 1], raman_hdpe$spectra[, 1])
    colnames(two) <- c("left", "right")
    two[1:10, "left"] <- NA
    two[(nrow(two) - 9):nrow(two), "right"] <- NA
    os <- as_OpenSpecy(raman_hdpe$wavenumber, two)

    processed <- manage_na(
        os,
        fun = function(x) {
            x$spectra <- x$spectra * 2
            attr(x, "derivative_order") <- "1"
            x
        }
    )

    expect_true(all(is.na(processed$spectra[1:10, "left"])))
    expect_true(all(is.na(processed$spectra[
        (nrow(two) - 9):nrow(two), "right"
    ])))
    expect_equal(processed$spectra[11, "left"], two[11, "left"] * 2)
    expect_equal(processed$spectra[1, "right"], two[1, "right"] * 2)
    expect_equal(attr(processed, "derivative_order"), "1")
})
