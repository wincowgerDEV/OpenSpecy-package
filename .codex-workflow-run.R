options(error = function() {
  traceback(20)
  quit(save = "no", status = 1L, runLast = FALSE)
})
sys.source("workflows/OpenSpecy_reference_library.R", envir = globalenv())
