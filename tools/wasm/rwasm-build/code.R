# Vendored from r-wasm/actions build-rwasm/code.R at commit
# 0f8493df20b6b47d3621f16be81218926a09dad1 (MIT license).
args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0L) stop("No args supplied to Rscript.")

image_path <- args[[1L]]
repo_path <- args[[2L]]
compress <- args[[3L]]
if (!nzchar(image_path) && !nzchar(repo_path)) {
  stop("At least one of image-path or repo-path must be supplied.")
}

packages <- strsplit(args[[4L]], "[[:space:],]+")[[1L]]
strip <- strsplit(args[[5L]], "[[:space:],]+")[[1L]]
if (length(strip) == 1L && identical(strip, "NULL")) strip <- NULL

cat("\nArgs:\n")
str(list(image_path = image_path, repo_path = repo_path,
         packages = packages, strip = strip))
if (!requireNamespace("withr", quietly = TRUE)) install.packages("withr")
withr::local_dir("/github/workspace")
withr::local_envvar(list(
  GITHUB_PAT = Sys.getenv("GITHUB_PAT", Sys.getenv("GITHUB_TOKEN"))
))

message("\n\nAdding packages:\n",
        paste0("* ", packages, collapse = "\n"))
rwasm::add_pkg(packages, repo_dir = repo_path, compress = compress)
message("\n\nMaking library")
rwasm::make_vfs_library(out_dir = image_path, repo_dir = repo_path,
                         strip = strip, compress = compress)

if (compress) {
  uncompressed <- list.files(image_path, pattern = "\\.data$",
                             full.names = TRUE)
  if (length(uncompressed)) file.remove(uncompressed)
}
