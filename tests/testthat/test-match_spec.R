# Create temp dir for testthat
tmp <- file.path(tempdir(), "OpenSpecy-testthat")
dir.create(tmp, showWarnings = F)

data("test_lib")

unknown <- read_extdata("ftir_ldpe_soil.asp") |> read_any() |>
  conform_spec(range = test_lib$wavenumber, res = spec_res(test_lib)) |>
  process_spec(smooth_intens = T, make_rel = T)

# Create a subset of test_lib for filtering
test_lib_extract <- filter_spec(
  test_lib, logic = test_lib$metadata$polymer_class == "polycarbonates"
  )

test_that("ai_classify() handles input errors correctly", {
  ai_classify(1:1000) |> expect_error()
})

test_that("match_spec() returns correct structure with AI", {
  skip_on_cran()
  skip_if_offline(host = "api.osf.io")

  get_lib("model", path = tmp)
  lib <- load_lib(type = "model", path = tmp)

  expect_error(check_OpenSpecy(lib))

  set.seed(47)
  rn <- runif(n = length(unique(lib$variables_in)))
  fill <- as_OpenSpecy(as.numeric(unique(lib$variables_in)),
                       spectra = data.frame(rn))
  matches <- match_spec(x = unknown, library = lib, na.rm = T, fill = fill) |>
    expect_silent()
  nrow(matches) |> expect_equal(1)
  names(matches) |> expect_contains(c("x", "y", "z", "value", "name"))
  round(matches$value, 2) |> expect_equal(0.52)
  grepl("polyamide", matches$name) |> expect_true()
})

test_that("match_spec() handles input errors correctly", {
  match_spec(1:1000) |> expect_error()
})

# Match_spec function
test_that("match_spec() returns correct structure", {
  matches <- match_spec(x = unknown, library = test_lib, na.rm = T, top_n = 5,
                        add_library_metadata = "sample_name",
                        add_object_metadata = "col_id") |>
    expect_silent()

  nrow(matches) |> expect_equal(5)
  names(matches) |> expect_contains(c("object_id", "library_id", "match_val"))
  round(matches$match_val, 2) |> expect_equal(c(0.67, 0.57, 0.55, 0.50, 0.43))
  tolower(matches$polymer) |> expect_equal(
    c("poly(ethylene)", "polystyrene", "poly(vinyl chloride)",
      "poly(dimethylsiloxane) (pdms)", NA)
  )
})

test_that("cor_spec() handles input errors correctly", {
  cor_spec(1:1000) |> expect_error()
  restrict_range(raman_hdpe, 300, 310) |> cor_spec(test_lib) |>
    expect_error()
})

# Write the tests for cor_spec function
test_that("cor_spec() returns a data.table with correct columns", {
  matches <- cor_spec(unknown, library = test_lib) |>
    expect_silent()

  unknown2 <- unknown
  unknown2$wavenumber[1:3] <- unknown2$wavenumber[1:3] + 1

  matches2 <- cor_spec(unknown2, library =  test_lib) |>
    expect_warning()
  inherits(matches, "matrix") |> expect_true()
  expect_identical(dim(matches), c(ncol(test_lib$spectra),
                                   ncol(unknown$spectra)))

  top_matches <- max_cor_named(cor_matrix = matches, na.rm = T) |>
    expect_silent()

  expect_length(top_matches, 1)

  ncol(filter_spec(test_lib, logic = names(top_matches))$spectra) |>
    expect_equal(1)

  test_lib$metadata$test <- NA

  test_metadata <- get_metadata(test_lib, logic = names(top_matches),
                                rm_empty = T) |>
    expect_silent()

  expect_equal(nrow(test_metadata), 1)
  full_test <- ident_spec(matches, unknown, library = test_lib, top_n = 5,
                          add_library_metadata = "sample_name") |>
    expect_silent()

  nrow(full_test) |> expect_equal(5)
  names(full_test) |> expect_contains(c("object_id", "library_id", "match_val"))
  round(full_test$match_val, 2) |> expect_equal(c(0.67, 0.57, 0.55, 0.50, 0.43))
  tolower(full_test$polymer) |> expect_equal(
    c("poly(ethylene)", "polystyrene", "poly(vinyl chloride)",
      "poly(dimethylsiloxane) (pdms)", NA)
  )
})

test_that("filter_spec() handles input errors correctly", {
  filter_spec(1:1000) |> expect_error()
})

# Write the tests for filter_spec function
test_that("filter_spec() returns erroneous OpenSpecy object when removing all spectra", {
  os_filtered <- filter_spec(test_lib, logic = rep(F, ncol(test_lib$spectra))) |>
    expect_silent()
  expect_warning(check_OpenSpecy(os_filtered))
  expect_equal(ncol(os_filtered$spectra), 0)
  expect_equal(nrow(os_filtered$metadata), 0)
})

# Write the tests for filter_spec function
test_that("filter_spec() returns OpenSpecy object with filtered spectra", {
  logic <- rep(F,ncol(test_lib$spectra))
  logic[1] <- TRUE

  os_filtered <- filter_spec(test_lib, logic = logic) |>
    expect_silent()
  expect_true(check_OpenSpecy(os_filtered))

  expect_equal(ncol(os_filtered$spectra), 1)
  expect_equal(nrow(os_filtered$metadata), 1)
})

test_that("get_metadata() handles input errors correctly", {
  get_metadata(1:1000) |> expect_error()
})

# Tidy up
unlink(tmp, recursive = T)
