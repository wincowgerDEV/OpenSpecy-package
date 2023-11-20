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
                    "centroid_x"))
})

test_that("collapse particles returns expected values", {
  skip_on_cran()

  particles <- ifelse(map$metadata$y == 1, "particleA", "particleB")
  id_map <- def_features(map, particles)
  expect_true(check_OpenSpecy(id_map))

  test_collapsed <- collapse_spec(id_map)

  expect_true(check_OpenSpecy(test_collapsed))

  test_collapsed$metadata |> nrow() |>
    expect_equal(3)
  test_collapsed$metadata$feret_max |> round(2) |>
    expect_equal(c(16.0, 16.0, 19.03))
  test_collapsed$metadata$centroid_x |> unique() |>
    expect_equal(7.5)

  particles <- map$metadata$y == 1
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

  expect_contains(names(test_collapsed$metadata),
                  c("feature_id", "area", "feret_max", "centroid_y",
                    "centroid_x"))
})
