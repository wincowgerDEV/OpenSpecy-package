map <- read_extdata("CA_tiny_map.zip") |> read_any()

test_that("features are identified when given logical", {
  map$metadata$particles <- map$metadata$y == 0
  id_map <- def_features(map, map$metadata$particles)
  expect_length(unique(id_map$metadata$feature_id), 2)
  expect_equal(max(id_map$metadata$area, na.rm = T), 13)
  expect_equal(max(id_map$metadata$feret_max, na.rm = T), 13)
  expect_equal(max(id_map$metadata$feret_min, na.rm = T), 1)
  expect_equal(max(id_map$metadata$perimeter, na.rm = T), 24)
})

test_that("particles are identified when given character", {
  map$metadata$particles <- ifelse(map$metadata$y == 1, "particle", "not_particle")
  id_map <- def_features(map, map$metadata$particles)
  expect_length(unique(id_map$metadata$feature_id), 3)
  expect_equal(max(id_map$metadata$area, na.rm = T), 182)
  expect_equal(round(max(id_map$metadata$feret_max, na.rm = T)), 19)
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
  map <- read_extdata("CA_tiny_map.zip") |> read_any()

  id_map <- def_features(map,ifelse(map$metadata$x == 1,
                                    "particle", "not_particle"))

  expect_equal(id_map$wavenumber, map$wavenumber)
  expect_equal(id_map$spectra, map$spectra)
  expect_contains(id_map$metadata, map$metadata)

  expect_contains(names(id_map$metadata),
                  c("feature_id", "area", "feret_max", "centroid_y",
                    "centroid_x"))
})

test_that("collapse particles returns expected values", {
  particles <- ifelse(map$metadata$y == 1, "particleA", "particleB")
  id_map <- def_features(map, particles)
  test_collapsed <- collapse_spec(id_map)

  expect_equal(test_collapsed$metadata |> nrow(), 3)
  expect_equal(test_collapsed$metadata$feret_max |> round(2), c(13, 13, 18.69))
  expect_equal(test_collapsed$metadata$centroid_x |> unique(), 6)

  particles <- map$metadata$y == 1
  id_map <- def_features(map, particles)
  test_collapsed <- collapse_spec(id_map)

  expect_equal(test_collapsed$metadata |> nrow(), 2)
  expect_equal(test_collapsed$metadata$feret_max |> round(2), c(NA, 13))
  expect_equal(test_collapsed$metadata$centroid_x |> unique(), 6)

  expect_contains(names(test_collapsed$metadata),
                  c("feature_id", "area", "feret_max", "centroid_y",
                    "centroid_x"))
})
