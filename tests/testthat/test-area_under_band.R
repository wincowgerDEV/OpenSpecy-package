data("raman_hdpe")

test_that("area_under_band() throws an errors", {
  # Create a non-OpenSpecy object
  area_under_band(x = "d", min = 100, max = 1000) |>
    expect_error()
    
    area_under_band(x = raman_hdpe) |>
        expect_error()
})

test_that("area_under_band() produces expected values", {
    # Create a non-OpenSpecy object
    expect_gt(area_under_band(x = raman_hdpe, min = 2700, max = 3000), area_under_band(x = raman_hdpe, min = 1500, max = 2000))
    expect_identical(unname(area_under_band(x = raman_hdpe, min = 2700, max = 3000)), 21421)
    expect_gt(1, area_under_band(x = raman_hdpe, min = 1500, max = 1600)/area_under_band(x = raman_hdpe, min = 1400, max = 1500))
    expect_gt(area_under_band(x = raman_hdpe, min = 1400, max = 1500)/ area_under_band(x = raman_hdpe, min = 1500, max = 1600), 1)
})

