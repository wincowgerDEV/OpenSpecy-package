# Create temp dir for testthat
tmp <- file.path(tempdir(), "OpenSpecy-testthat")
dir.create(tmp, showWarnings = F)

data("test_lib")
data("raman_hdpe")

CA_test_lib <- filter_spec(test_lib, test_lib$metadata$SpectrumIdentity == "CA" )

hdpe_test_lib <- filter_spec(test_lib, test_lib$metadata$sample_name == "0031bb13faea1e04b52ffbeca009e8ab")

tiny_map <- read_extdata("CA_tiny_map.zip") |>
  read_any() |>
  conform_spec(range = test_lib$wavenumber,
               res = NULL) |>
  process_spec(smooth_intens = T, make_rel = T)

unknown <- read_extdata("ftir_ldpe_soil.asp") |> read_any()
preproc <- conform_spec(unknown, range = test_lib$wavenumber,
                        res = spec_res(test_lib)) |>
  process_spec(smooth_intens = T, make_rel = T)

test_that("os_similarity() returns correct values", {
    #The basic definition of similarity for each
    expect_true(os_similarity(tiny_map, tiny_map, method = "hamming") == 1) 
    expect_true(os_similarity(tiny_map, tiny_map, method = "pca") == 1) 
    expect_true(os_similarity(tiny_map, tiny_map, method = "metadata") == 1) 
    expect_true(os_similarity(tiny_map, tiny_map, method = "wavenumber") == 1) 
    
    #Wavenumbers
    expect_identical(os_similarity(x = tiny_map, y = test_lib, method = "wavenumber") |> round(2), 0.84)
    expect_identical(os_similarity(x = tiny_map, y = tiny_map, method = "wavenumber") |> round(2), 1)
    expect_true(os_similarity(x = tiny_map, y = test_lib, method = "wavenumber") > os_similarity(x = unknown, y = test_lib, method = "wavenumber"))
    
    test_lib2 <- conform_spec(test_lib, tiny_map$wavenumber, res = NULL, type = "roll") 
    
    CA2 <- conform_spec(CA_test_lib, tiny_map$wavenumber, res = NULL, type = "roll") 
    
    hdpe2 <- conform_spec(hdpe_test_lib, tiny_map$wavenumber, res = NULL, type = "roll") 
    
    unknown2 <- conform_spec(unknown, tiny_map$wavenumber, res = NULL, type = "roll") 
    
    ramanhdpe2 <- conform_spec(raman_hdpe, tiny_map$wavenumber, res = NULL, type = "roll") |>
        smooth_intens()
    
    #hamming still calculates a value for single spectra comparisons but probably not a great metric. 
    expect_true(os_similarity(ramanhdpe2, hdpe2) > os_similarity(ramanhdpe2,CA2)) |> expect_warning() |> expect_warning()
    
    os_similarity(ramanhdpe2, hdpe2, method = "pca") |> expect_error() |> expect_warning()
    
    expect_true(os_similarity(test_lib2, test_lib2) > os_similarity(tiny_map, test_lib2))
    expect_true(os_similarity(tiny_map, CA2) > os_similarity(tiny_map, unknown2)) |> expect_warning()
    expect_true(os_similarity(tiny_map, CA2, method = "pca") > os_similarity(x = tiny_map, y = unknown2, method = "pca")) |> expect_warning()
    expect_true(os_similarity(tiny_map, raman_hdpe, method = "metadata") == 0.25)
})

test_that("ai_classify() handles input errors correctly", {
  ai_classify(1:1000) |> expect_error()
})


test_that("match_spec() returns correct structure with AI", {
  skip_on_cran()
  skip_if_offline(host = "api.osf.io")

  get_lib("model_derivative", path = tmp)
  lib <- load_lib(type = "model_derivative", path = tmp)

  check_OpenSpecy(lib) |>
    expect_error() |> expect_warning() |> expect_warning() |>
    expect_warning() |> expect_warning() |> expect_warning()

  set.seed(47)
  rn <- runif(n = length(unique(lib$all_variables)))
  fill <- as_OpenSpecy(as.numeric(unique(lib$all_variables)),
                       spectra = data.frame(rn))
  
  preproc2 <- conform_spec(unknown, range = fill$wavenumber,
                          res = NULL) |>
      smooth_intens() #%>%
      #restrict_range(min = 900, max = 3000)
  
  matches <- match_spec(x = preproc2, library = lib, na.rm = T, fill = fill) |>
    expect_silent()
  
  nrow(matches) |> expect_equal(1)
  names(matches) |> expect_contains(c("x", "y", "z", "value", "name"))
  #round(matches$value, 2) |> expect_equal(0.37)
  #grepl("polyolefin", matches$name) |> expect_true()
})

test_that("match_spec() handles input errors correctly", {
  match_spec(1:1000) |> expect_error()
})

test_that("match_spec() handles attribute issues correctly", {
  preproc_wa <- as_OpenSpecy(preproc$wavenumber,
                             preproc$spectra,
                             preproc$metadata[,-c("x", "y")],
                             attributes = list(intensity_unit = "absorbance",
                                               derivative_order = 1,
                                               baseline = "nobaseline",
                                               spectra_type = "ftir"))

  test_lib_wa <- as_OpenSpecy(test_lib$wavenumber,
                              test_lib$spectra,
                              test_lib$metadata[,-c("x", "y")],
                              attributes = list(intensity_unit = "absorbance",
                                                derivative_order = 1,
                                                baseline = "nobaseline",
                                                spectra_type = "ftir"))

  match_spec(x = preproc_wa, library = test_lib_wa, na.rm = T, top_n = 5,
             add_library_metadata = "sample_name",
             add_object_metadata = "col_id") |>
    expect_silent()

  test_lib_wa2 <- as_OpenSpecy(test_lib$wavenumber,
                               test_lib$spectra,
                               test_lib$metadata[,-c("x", "y")],
                               attributes = list(intensity_unit = "transmittance",
                                                 derivative_order = 1,
                                                 baseline = "nobaseline",
                                                 spectra_type = "ftir"))

  match_spec(x = preproc_wa, library = test_lib_wa2, na.rm = T, top_n = 5,
             add_library_metadata = "sample_name",
             add_object_metadata = "col_id") |>
    expect_warning()

  test_lib_wa3 <- as_OpenSpecy(test_lib$wavenumber,
                               test_lib$spectra,
                               test_lib$metadata[,-c("x", "y")],
                               attributes = list(intensity_unit = "absorbance",
                                                 derivative_order = 2,
                                                 baseline = "nobaseline",
                                                 spectra_type = "ftir"))

  match_spec(x = preproc_wa, library = test_lib_wa3, na.rm = T, top_n = 5,
             add_library_metadata = "sample_name",
             add_object_metadata = "col_id") |>
    expect_warning()

  test_lib_wa4 <- as_OpenSpecy(test_lib$wavenumber,
                               test_lib$spectra,
                               test_lib$metadata[,-c("x", "y")],
                               attributes = list(intensity_unit = "absorbance",
                                                 derivative_order = 1,
                                                 baseline = "raw",
                                                 spectra_type = "ftir"))

  match_spec(x = preproc_wa, library = test_lib_wa4, na.rm = T, top_n = 5,
             add_library_metadata = "sample_name",
             add_object_metadata = "col_id") |>
    expect_warning()

  test_lib_wa5 <- as_OpenSpecy(test_lib$wavenumber,
                               test_lib$spectra,
                               test_lib$metadata[,-c("x", "y")],
                               attributes = list(intensity_unit = "absorbance",
                                                 derivative_order = 1,
                                                 baseline = "nobaseline",
                                                 spectra_type = "raman"))

  match_spec(x = preproc_wa, library = test_lib_wa5, na.rm = T, top_n = 5,
             add_library_metadata = "sample_name",
             add_object_metadata = "col_id") |>
    expect_warning()
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
test_that("filter_spec() does not allow for OpenSpecy object without spectra", {
  os_filtered <- filter_spec(test_lib, logic = rep(F, ncol(test_lib$spectra))) |>
    expect_error()
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

test_that("cor_spec() routine with preprocessing returns same values as setting conform = T", {
  tiny_map2 <- read_extdata("CA_tiny_map.zip") |>
    read_any() |>
    process_spec(smooth_intens = T, conform_spec = F, make_rel = T)

  tiny_map3 <- tiny_map2 |>
    conform_spec(range = test_lib$wavenumber, res = NULL, type = "roll")

  cors <- cor_spec(tiny_map3, test_lib)
  cors2 <- cor_spec(tiny_map2, test_lib, conform = T, type = "roll")
  expect_identical(cors, cors2)
  
  
  tiny_map3 <- tiny_map2 |>
      conform_spec(range = test_lib$wavenumber, res = NULL, type = "interp")
  
  cors <- cor_spec(tiny_map3, test_lib)
  cors2 <- cor_spec(tiny_map2, test_lib, conform = T, type = "interp")
  expect_identical(cors, cors2)
  
})

# Tidy up
unlink(tmp, recursive = T)
