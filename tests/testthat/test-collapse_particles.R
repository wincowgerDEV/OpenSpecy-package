
test_that("check that particles are identified when given logical", {
    map <- read_extdata("CA_tiny_map.zip") |> read_any()
    map$metadata$particles <- map$metadata$x == 0
    identified_map <- characterize_particles(map, map$metadata$particles)
    expect_true(is_OpenSpecy(identified_map))
    expect_true(length(unique(identified_map$metadata$particle_ids)) == 2)
    expect_true(max(identified_map$metadata$area, na.rm = T) == 13)
    expect_true(max(identified_map$metadata$feret_max, na.rm = T) == 13)
})

test_that("check that particles are identified when given character", {
    map <- read_extdata("CA_tiny_map.zip") |> read_any()
    map$metadata$particles <- ifelse(map$metadata$x == 1, "particle", "not_particle")
    identified_map <- characterize_particles(map, map$metadata$particles)
    expect_true(is_OpenSpecy(identified_map))
    expect_true(length(unique(identified_map$metadata$particle_ids)) == 3)
    expect_true(max(identified_map$metadata$area, na.rm = T) == 182)
    expect_true(round(max(identified_map$metadata$feret_max, na.rm = T)) == 19)
})


test_that("check that an error is thrown for invalid 'particles' input", {
    map <- read_extdata("CA_tiny_map.zip") |> read_any()
    expect_error(characterize_particles(map, map$metadata), 
                 "Particles needs to be a character or logical vector.")
})

test_that("check that particles are identified with all TRUE or FALSE logical vectors", {
    map <- read_extdata("CA_tiny_map.zip") |> read_any()
    
    # All TRUE case
    map$metadata$particles <- rep(TRUE, nrow(map$metadata))
    expect_error(identified_map <- characterize_particles(map, map$metadata$particles))
    
    # All FALSE case
    map$metadata$particles <- rep("test_FALSE", nrow(map$metadata))
    expect_error(identified_map <- characterize_particles(map, map$metadata$particles))
})

test_that("check that the original OpenSpecy object remains unmodified", {
    map <- read_extdata("CA_tiny_map.zip") |> read_any()
    map2 <- map

    particles <- ifelse(map$metadata$x == 1, "particle", "not_particle")
    identified_map <- characterize_particles(map, particles)
    
    expect_equal(map, map2)
})

test_that("check that collapse particles returns expected values", {
map <- read_extdata("CA_tiny_map.zip") |> read_any()
particles <- ifelse(map$metadata$x == 1, "particleA", "particleB")
identified_map <- characterize_particles(map, particles)
test_collapsed <- collapse_spectra(identified_map)
expect_true(is_OpenSpecy(test_collapsed))

map <- read_extdata("CA_tiny_map.zip") |> read_any()
particles <- map$metadata$x == 1
identified_map <- characterize_particles(map, particles)
test_collapsed <- collapse_spectra(identified_map)
expect_true(is_OpenSpecy(test_collapsed))

expect_equal(c("particle_ids", "area", "feret_max", "centroid_y", "centroid_x"), names(test_collapsed$metadata))

})


