test_that("bundled package data preserve clean text and package license", {
  data("test_lib", package = "OpenSpecy", envir = environment())
  data("raman_hdpe", package = "OpenSpecy", envir = environment())

  pigment_note <- test_lib$metadata$OtherInformation[
    grepl("Pigments Checker version 3.0", test_lib$metadata$OtherInformation,
          fixed = TRUE)
  ]
  expect_length(pigment_note, 1L)
  expect_match(pigment_note, "123-132", fixed = TRUE)
  expect_false(grepl(intToUtf8(c(0x00e2, 0x0080, 0x0093)), pigment_note,
                     fixed = TRUE))

  varnish_note <- test_lib$metadata$OtherInformation[
    grepl("Metall Schutzlack", test_lib$metadata$OtherInformation,
          fixed = TRUE)
  ]
  expect_length(varnish_note, 1L)
  expect_match(varnish_note, paste0("dunkelgr", intToUtf8(0x00fc), "n"),
               fixed = TRUE)
  expect_false(grepl(intToUtf8(c(0x251c, 0x255d)), varnish_note,
                     fixed = TRUE))

  expect_identical(raman_hdpe$metadata$license, "CC BY 4.0")
  expect_identical(raman_hdpe$metadata$organization, "Horiba Scientific")

  alternate <- lapply(
    c("raman_hdpe.json", "raman_hdpe.rds", "raman_hdpe_os.csv"),
    function(name) read_spec(read_extdata(name))
  )
  expect_true(all(vapply(alternate, function(object) {
    identical(object$metadata$license, "CC BY 4.0")
  }, logical(1))))
})
