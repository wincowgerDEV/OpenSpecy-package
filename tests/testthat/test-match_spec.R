# Create temp dir for testthat
tmp <- file.path(tempdir(), "OpenSpecy-testthat")
dir.create(tmp, showWarnings = F)

data("test_lib")

tiny_map <- read_extdata("CA_tiny_map.zip") |>
  read_any() |>
  conform_spec(range = test_lib$wavenumber,
               res = NULL) |>
  process_spec(smooth_intens = T, make_rel = T)

unknown <- read_extdata("ftir_ldpe_soil.asp") |> read_any()
preproc <- conform_spec(unknown, range = test_lib$wavenumber,
                        res = spec_res(test_lib)) |>
  process_spec(smooth_intens = T, make_rel = T)

test_that("ai_classify() handles input errors correctly", {
  ai_classify(1:1000) |> expect_error()
})

test_that("match_spec() returns correct structure with AI", {
  skip_on_cran()
  skip_if_offline(host = "api.osf.io")

  get_lib("model", path = tmp)
  lib <- load_lib(type = "model", path = tmp)

  check_OpenSpecy(lib) |>
    expect_error() |> expect_warning() |> expect_warning() |>
    expect_warning() |> expect_warning() |> expect_warning()

  set.seed(47)
  rn <- runif(n = length(unique(lib$variables_in)))
  fill <- as_OpenSpecy(as.numeric(unique(lib$variables_in)),
                       spectra = data.frame(rn))
  matches <- match_spec(x = preproc, library = lib, na.rm = T, fill = fill) |>
    expect_silent()
  nrow(matches) |> expect_equal(1)
  names(matches) |> expect_contains(c("x", "y", "z", "value", "name"))
  round(matches$value, 2) |> expect_equal(0.52)
  grepl("polyamide", matches$name) |> expect_true()
})

test_that("match_spec() handles input errors correctly", {
  match_spec(1:1000) |> expect_error()
})

test_that("match_spec() returns correct structure", {
  matches <- match_spec(x = preproc, library = test_lib, na.rm = T, top_n = 5,
                        add_library_metadata = "sample_name",
                        add_object_metadata = "col_id") |>
    expect_silent()

  order <- match_spec(x = preproc, library = test_lib, na.rm = T, top_n = 5,
                      order = unknown,
                      add_library_metadata = "sample_name",
                      add_object_metadata = "col_id") |>
    expect_silent()

  nrow(matches) |> expect_equal(5)
  names(matches) |> expect_contains(c("object_id", "library_id", "match_val"))
  round(matches$match_val, 2) |> expect_equal(c(0.57, 0.67, 0.55, 0.43, 0.50))
  tolower(matches$polymer) |> expect_equal(
    c("polystyrene", "poly(ethylene)", "poly(vinyl chloride)", NA,
      "poly(dimethylsiloxane) (pdms)")
  )
  expect_equal(matches, order)
})

test_that("cor_spec() handles input errors correctly", {
  cor_spec(1:1000) |> expect_error()
  restrict_range(raman_hdpe, 300, 310) |> cor_spec(test_lib) |>
    expect_error()
})

# Write the tests for cor_spec function
test_that("cor_spec() returns a data.table with correct columns", {
  skip_on_cran()

  matches <- cor_spec(preproc, library = test_lib) |>
    expect_silent()

  preproc2 <- preproc
  preproc2$wavenumber[1:3] <- preproc2$wavenumber[1:3] + 1

  cor_spec(preproc2, library =  test_lib) |>
    expect_warning()

  inherits(matches, "matrix") |> expect_true()
  expect_identical(dim(matches), c(ncol(test_lib$spectra),
                                   ncol(preproc$spectra)))

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
  full_test <- ident_spec(matches, preproc, library = test_lib, top_n = 5,
                          add_library_metadata = "sample_name") |>
    expect_silent()

  nrow(full_test) |> expect_equal(5)
  names(full_test) |> expect_contains(c("object_id", "library_id", "match_val"))
  round(full_test$match_val, 2) |> expect_equal(c(0.57, 0.67, 0.55, 0.43, 0.50))
  tolower(full_test$polymer) |> expect_equal(
    c("polystyrene", "poly(ethylene)", "poly(vinyl chloride)", NA,
      "poly(dimethylsiloxane) (pdms)")
  )
})

test_that("filter_spec() handles input errors correctly", {
  filter_spec(1:1000) |> expect_error()
})

test_that("Test that raman hdpe accurately identified", {
  proc_rhdpe <- process_spec(raman_hdpe,conform_spec = T,
                             conform_spec_args = list(range = test_lib$wavenumber,
                                                      res = NULL,
                                                      type = "interp"))

  check <- match_spec(proc_rhdpe, test_lib, top_n = 1,
                      add_library_metadata = "sample_name")

  expect_identical(round(check$match_val, 3), 0.974)
  expect_identical(check$polymer_class, "Polyolefins (POLYALKENES)")
})

# Write the tests for filter_spec function
test_that("filter_spec() returns erroneous OpenSpecy object when removing all spectra", {
  os_filtered <- filter_spec(test_lib, logic = rep(F, ncol(test_lib$spectra))) |>
    expect_silent()
  check_OpenSpecy(os_filtered) |> expect_warning() |> expect_warning()
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

test_that("cor_spec() routine and match_spec() return same values", {
  cors <- cor_spec(tiny_map, test_lib)
  max_correlations <- max_cor_named(cors)
  names <- max_correlations |> sort(decreasing = T) |> names()
  top_matches <- match_spec(x = tiny_map, library = test_lib, top_n = 1)
  expect_identical(names, top_matches$library_id)
  top_matches_2 <- match_spec(x = tiny_map, library = test_lib, top_n = 2)[, head(.SD, 1), by = "object_id"]
  expect_identical(names, top_matches$library_id)
})

# Tidy up
unlink(tmp, recursive = T)
