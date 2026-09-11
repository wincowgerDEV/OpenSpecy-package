options(error = function() {
  traceback(20)
  quit(save = "no", status = 1L, runLast = FALSE)
})

devtools::load_all(quiet = TRUE)

result <- rebuild_lib_artifacts(
  x = "C:/Users/winco/OneDrive/Documents/OpenSpecy_offline/reference-library-build",
  output_dir = "C:/Users/winco/OneDrive/Documents/OpenSpecy_offline/reference-library-build",
  previous_library_dir = "system",
  reuse = TRUE,
  seed = 123,
  holdout = 0.1,
  progress = TRUE
)

message("Downstream rebuild complete: ", attr(result, "output_dir"))
