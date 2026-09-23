devtools::load_all(quiet = TRUE)

directory <- file.path(tempdir(), "openspecy-latest-aws")
dir.create(directory)
types <- c(
  "medoid_derivative", "medoid_nobaseline",
  "model_derivative", "model_nobaseline"
)
get_lib(types, path = directory, quiet = TRUE)
for (type in types) {
  file <- file.path(directory, paste0(type, ".rds"))
  object <- load_lib(type, path = directory)
  cat(
    type, unname(file.info(file)$size),
    digest::digest(file, algo = "sha256", file = TRUE),
    paste(names(object), collapse = ","), "\n"
  )
}
