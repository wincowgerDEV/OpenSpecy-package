patch_shinylive_workerfs <- function(site_dir) {
  bundle <- file.path(site_dir, "shinylive", "shinylive.js")
  if (!file.exists(bundle)) {
    stop("Cannot patch missing Shinylive bundle: ", bundle, call. = FALSE)
  }
  expected_sha256 <-
    "3c0f0352fa7dcdb190b30b49b640a68520efc6f4b7468b6dd26ec19bc1de6ed4"
  actual_sha256 <- digest::digest(bundle, algo = "sha256", file = TRUE,
                                  serialize = FALSE)
  if (!identical(tolower(actual_sha256), expected_sha256)) {
    stop(
      "Pinned Shinylive asset hash mismatch; refusing to inject the ",
      "WORKERFS bridge. Found ", actual_sha256, ".",
      call. = FALSE
    )
  }

  source <- rawToChar(readBin(bundle, what = "raw", n = file.info(bundle)$size))
  eager <- paste0(
    '"packages" in t2 && t2.packages && (n = [...n, ',
    '...t2.packages.map((a) => a.blob instanceof Blob ? ',
    'a.blob.arrayBuffer()'
  )
  lazy <- paste0(
    'e2 !== "WORKERFS" && "packages" in t2 && t2.packages && ',
    '(n = [...n, ...t2.packages.map((a) => a.blob instanceof Blob ? ',
    'a.blob.arrayBuffer()'
  )
  eager_hits <- gregexpr(eager, source, fixed = TRUE)[[1L]]
  if (length(eager_hits) != 1L || eager_hits[[1L]] < 0L) {
    stop(
      "Pinned Shinylive WORKERFS conversion anchor changed (matches: ",
      if (eager_hits[[1L]] < 0L) 0L else length(eager_hits), ").",
      call. = FALSE
    )
  }
  source <- sub(eager, lazy, source, fixed = TRUE)

  anchor <- "var webRProxyHandlePromise = null;"
  hits <- gregexpr(anchor, source, fixed = TRUE)[[1L]]
  if (length(hits) != 1L || hits[[1L]] < 0L) {
    stop("Pinned Shinylive proxy anchor changed.", call. = FALSE)
  }
  bridge_file <- file.path("tools", "wasm", "shinylive-workerfs-bridge.js")
  bridge <- rawToChar(readBin(
    bridge_file, what = "raw", n = file.info(bridge_file)$size
  ))
  source <- sub(anchor, paste(anchor, bridge, sep = "\n"), source,
                fixed = TRUE)
  connection <- file(bundle, open = "wb")
  on.exit(close(connection), add = TRUE)
  writeBin(charToRaw(source), connection)
  invisible(bundle)
}
