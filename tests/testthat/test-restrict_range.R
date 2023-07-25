test_that("restric range provides correct range", {
     test_noise = as_OpenSpecy(x = seq(400,4000, by = 10), spectra = data.table(intensity = rnorm(361)))
     expect_silent(single_range <- restrict_range(test_noise, min_range = 1000, max_range = 2000))
     expect_silent(double_range <- restrict_range(test_noise, min_range = c(1000, 2000) , max_range = c(1500, 2500)))
     expect_true(is_OpenSpecy(single_range))
     expect_true(is_OpenSpecy(double_range))
     expect_identical(single_range$wavenumber, seq(1000,2000, by = 10))
     expect_identical(double_range$wavenumber, c(seq(1000,1500, by = 10), seq(2000,2500, by = 10)))
})
