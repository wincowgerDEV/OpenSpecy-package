library(jpeg)

map <- read_extdata("CA_tiny_map.zip") |> read_any()

test_that("def_features() handles input errors correctly", {
  def_features(1:1000) |> expect_error()
})

test_that("collapse_spec() handles input errors correctly", {
  collapse_spec(1:1000) |> expect_error()
})

test_that("features are identified when given logical", {
  map$metadata$particles <- map$metadata$y == 0
  id_map <- def_features(map, map$metadata$particles)
  check_OpenSpecy(id_map) |> expect_true()
  unique(id_map$metadata$feature_id) |> expect_length(2)
  max(id_map$metadata$area, na.rm = T) |> expect_equal(16)
  max(id_map$metadata$feret_max, na.rm = T) |> round(2) |>
    expect_equal(16)
  max(id_map$metadata$feret_min, na.rm = T) |> round(2) |>
    expect_equal(1)
  max(id_map$metadata$perimeter, na.rm = T) |> round(2) |>
    expect_equal(30)
})

test_that("features are identified with sig_noise and smoothing with closing", {
    map$metadata$snr <- sig_noise(map, metric = "noise")
    #heatmap_spec(map, map$metadata$snr)
    id_map <- def_features(map, map$metadata$snr > 0.1)
    check_OpenSpecy(id_map) |> expect_true()
    unique(id_map$metadata$feature_id) |> expect_length(2)
    #heatmap_spec(id_map, id_map$metadata$feature_id)

    #Less resolved sig
    map$metadata$snr <- sig_noise(map, metric = "sig_times_noise")
    #heatmap_spec(map, map$metadata$snr)
    id_map <- def_features(map, map$metadata$snr > 0.1, close = T, close_kernel = c(3,3))
    #heatmap_spec(id_map, id_map$metadata$feature_id)
    id_map2 <- def_features(map, map$metadata$snr > 0.1, close = F, close_kernel = c(3,3))
    #heatmap_spec(id_map2, id_map2$metadata$feature_id)
    expect_false(identical(id_map, id_map2))
    id_map3 <- def_features(map, map$metadata$snr > 0.1, close = T, close_kernel = c(5,5))
    #heatmap_spec(id_map3, id_map3$metadata$feature_id)
    expect_false(identical(id_map, id_map3))
    id_map4 <- def_features(map, map$metadata$snr > 0.1, close = T, close_kernel = c(6,6))
    #heatmap_spec(id_map4, id_map4$metadata$feature_id)
    expect_false(identical(id_map3, id_map4))
    
    #Test collapsing on binary
    test_part_close <- rep_len(F, length.out = ncol(map$spectra))
    test_part_close[c(69, 101,103)] <- T  
    #heatmap_spec(id_map4, test_part_close)
    
    id_map5 <- def_features(map, test_part_close, close = T, close_kernel = c(3,3))
    #heatmap_spec(id_map5, id_map5$metadata$feature_id)
    unique(id_map5$metadata$feature_id) |> expect_length(2)
    
    #Test collapsing on character
    test_part_close <- rep_len("background", length.out = ncol(map$spectra))
    test_part_close[c(69, 101,103, 104)] <- "particle1"  
    test_part_close[c(68, 70, 71, 87, 119, 118, 117, 100)] <- "particle2" 
    
    #heatmap_spec(map, test_part_close)
    
    id_map5 <- def_features(map, test_part_close, close = T, close_kernel = c(3,3))
    expect_true(nrow(id_map5$metadata) == ncol(id_map5$spectra))
    
    #heatmap_spec(id_map5, id_map5$metadata$feature_id)
    
    expect_true(is_OpenSpecy(id_map5))
    unique(id_map5$metadata$feature_id) |> expect_length(3)
    
    #Test collapsing on character complete overlap
    test_part_close <- rep_len("background", length.out = ncol(map$spectra))
    test_part_close[c(69, 101,103)] <- "particle1"  
    test_part_close[c(68, 70, 71, 87, 119, 118, 117, 100)] <- "particle2" 
    
    #heatmap_spec(map, test_part_close)
    
    id_map5 <- def_features(map, test_part_close, close = T, close_kernel = c(3,3))
    expect_true(nrow(id_map5$metadata) == ncol(id_map5$spectra))
    
    #heatmap_spec(id_map5, id_map5$metadata$feature_id)
    
    expect_true(is_OpenSpecy(id_map5))
    unique(id_map5$metadata$feature_id) |> expect_length(2)

})

test_that("particles are identified when given character", {
  map$metadata$particles <- ifelse(map$metadata$y == 1, "particle", "not_particle")
  id_map <- def_features(map, map$metadata$particles)
  expect_true(check_OpenSpecy(id_map))
  unique(id_map$metadata$feature_id) |>
    expect_length(3)
  max(id_map$metadata$area, na.rm = T) |>
    expect_equal(176)
  max(id_map$metadata$feret_max, na.rm = T) |> round(2) |>
    expect_equal(19.03)
})

test_that("an error is thrown for invalid feature input", {
  def_features(map, map$metadata) |> expect_error()
})

test_that("check that particles are identified with all TRUE or FALSE logical vectors", {
  # All TRUE case
  map$metadata$particles <- rep(TRUE, nrow(map$metadata))
  def_features(map, map$metadata$particles) |> expect_error()

  # All FALSE case
  map$metadata$particles <- rep("test_FALSE", nrow(map$metadata))
  def_features(map, map$metadata$particles) |> expect_error()
})

test_that("the original spectrum remains unmodified and metadata is amended", {
  skip_on_cran()

  map <- read_extdata("CA_tiny_map.zip") |> read_any()

  id_map <- def_features(map,ifelse(map$metadata$x == 1,
                                    "particle", "not_particle"))

  expect_true(check_OpenSpecy(id_map))

  expect_equal(id_map$wavenumber, map$wavenumber)
  expect_equal(id_map$spectra, map$spectra)
  expect_contains(id_map$metadata, map$metadata)

  expect_contains(names(id_map$metadata),
                  c("feature_id", "area", "feret_max", "centroid_y",
                    "centroid_x", "first_x", "first_y", "rand_x",  "rand_y"))
  
  
})

test_that("collapse particles returns expected values", {
  skip_on_cran()

  particles <- ifelse(map$metadata$y == 1, "particleA", "particleB")
  id_map <- def_features(map, particles)
  expect_true(check_OpenSpecy(id_map))

  test_collapsed <- collapse_spec(id_map)
  
  test_param2 <- collapse_spec(id_map, mean) |> expect_silent()
  
  check_OpenSpecy(test_param2) |> expect_true()
  
  test_param1 <- collapse_spec(id_map, mean, column = "feret_max") |> expect_silent()

  check_OpenSpecy(test_param1) |> expect_true()
  
  expect_true(check_OpenSpecy(test_collapsed))

  test_collapsed$metadata |> nrow() |>
    expect_equal(3)
  test_collapsed$metadata$feret_max |> round(2) |>
    expect_equal(c(16.0, 16.0, 19.03))
  test_collapsed$metadata$centroid_x |> unique() |>
    expect_equal(7.5)

  particles <- map$metadata$y == 1
  set.seed(10)
  id_map <- def_features(map, particles)
  expect_true(check_OpenSpecy(id_map))

  test_collapsed <- collapse_spec(id_map)
  check_OpenSpecy(test_collapsed) |> expect_true()

  test_collapsed$metadata |> nrow() |>
    expect_equal(2)
  test_collapsed$metadata$feret_max |> round(2) |>
    expect_equal(c(NA, 16))
  test_collapsed$metadata$centroid_x |> unique() |>
    expect_equal(7.5)
  
  test_collapsed$metadata$first_x |> unique() |> expect_equal(0)
  test_collapsed$metadata$first_y |> unique() |> expect_identical(c(0, 1))
  test_collapsed$metadata$rand_y |> unique() |> expect_identical(c(7, 1))
  test_collapsed$metadata$rand_x |> unique() |> expect_identical(c(8, 9))
  
  expect_contains(names(test_collapsed$metadata),
                  c("feature_id", "area", "feret_max", "centroid_y",
                    "centroid_x"))
})


# Create a synthetic image and OpenSpecy object for testing
create_synthetic_data <- function(x_dim, y_dim) {
    # Create a simple synthetic image with half white and half blue
    img <- array(0, dim = c(y_dim, x_dim, 3))
    img[, 1:(x_dim/2), ] <- 1  # White half
    img[, ((x_dim/2)+1):x_dim, 3] <- 1  # Blue half
    img_path <- tempfile(fileext = ".jpg")
    writeJPEG(img, target = img_path)
    
    img_path
}

# Generate synthetic data
synthetic_data <- create_synthetic_data(x_dim = max(map$metadata$x) +1, y_dim = max(map$metadata$y) +1)

test_that("RGB values are correctly extracted and stored in metadata", {
    particles <- ifelse(map$metadata$x == 15, TRUE, FALSE)
    
    id_map <- def_features(map, particles, img = synthetic_data, bottom_left = c(1, max(map$metadata$y) +1), top_right = c( max(map$metadata$x) +1, 1))
    expect_true(check_OpenSpecy(id_map))
    
    test_collapsed <- collapse_spec(id_map)
    expect_true(check_OpenSpecy(test_collapsed))
    
    expect_contains(names(test_collapsed$metadata), c("r", "g", "b"))
    
    test_collapsed$metadata[feature_id == "1", c("r", "g", "b")] |> unlist() |> unname() |> expect_equal(c(1, 0, 254))
    
    particles <- ifelse(map$metadata$x == 1, TRUE, FALSE)
    
    id_map <- def_features(map, particles, img = synthetic_data, bottom_left = c(1, max(map$metadata$y) +1), top_right = c( max(map$metadata$x) +1, 1))
    expect_true(check_OpenSpecy(id_map))
    
    test_collapsed <- collapse_spec(id_map)
    expect_true(check_OpenSpecy(test_collapsed))
    
    expect_contains(names(test_collapsed$metadata), c("r", "g", "b"))
    
    test_collapsed$metadata[feature_id == "1", c("r", "g", "b")] |> unlist() |> unname() |> expect_equal(c(254, 255, 255))

})
