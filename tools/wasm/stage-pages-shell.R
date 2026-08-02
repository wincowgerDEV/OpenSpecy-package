#!/usr/bin/env Rscript

# Assemble the hand-authored Pages shell around independently generated
# /app/ and /pkgdown/ trees. This helper intentionally copies only known
# landing sources and assets; it never removes or rewrites those two trees.

script_path <- function() {
  file_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  if (!length(file_arg)) return(NA_character_)
  normalizePath(sub("^--file=", "", file_arg[[1]]), winslash = "/",
                mustWork = TRUE)
}

repo_root_from_script <- function() {
  path <- script_path()
  if (is.na(path)) return(normalizePath(".", winslash = "/", mustWork = TRUE))
  normalizePath(file.path(dirname(path), "..", ".."), winslash = "/",
                mustWork = TRUE)
}

normalize_future_path <- function(path) {
  normalizePath(path, winslash = "/", mustWork = FALSE)
}

path_is_within <- function(path, parent) {
  path <- tolower(normalize_future_path(path))
  parent <- tolower(normalize_future_path(parent))
  identical(path, parent) || startsWith(path, paste0(parent, "/"))
}

copy_file_checked <- function(source, destination) {
  dir.create(dirname(destination), recursive = TRUE, showWarnings = FALSE)
  copied <- file.copy(source, destination, overwrite = TRUE, copy.mode = TRUE,
                      copy.date = TRUE)
  if (!isTRUE(copied)) {
    stop("Could not stage ", source, " at ", destination, call. = FALSE)
  }
}

copy_tree <- function(source_dir, destination_dir) {
  relative <- list.files(
    source_dir,
    recursive = TRUE,
    all.files = TRUE,
    no.. = TRUE,
    include.dirs = FALSE,
    full.names = FALSE
  )
  if (!length(relative)) {
    stop("Landing source is empty: ", source_dir, call. = FALSE)
  }

  protected <- c("app", "pkgdown")
  top_level <- tolower(vapply(
    strsplit(gsub("\\\\", "/", relative), "/", fixed = TRUE),
    `[[`, character(1), 1L
  ))
  if (any(top_level %in% protected)) {
    stop(
      "The landing source must not contain app/ or pkgdown/; those routes ",
      "are assembled independently.",
      call. = FALSE
    )
  }

  for (path in relative) {
    copy_file_checked(file.path(source_dir, path),
                      file.path(destination_dir, path))
  }
  invisible(relative)
}

redirect_document <- function(relative_target, canonical_target, label) {
  c(
    "<!doctype html>",
    "<html lang=\"en\">",
    "<head>",
    "  <meta charset=\"utf-8\">",
    paste0("  <title>", label, " moved | OpenSpecy</title>"),
    "  <meta name=\"robots\" content=\"noindex, follow\">",
    paste0("  <link rel=\"canonical\" href=\"", canonical_target, "\">"),
    paste0("  <meta http-equiv=\"refresh\" content=\"0; url=", relative_target,
           "\">"),
    "</head>",
    "<body>",
    paste0("  <p>This OpenSpecy page moved to <a href=\"", relative_target,
           "\">", label, "</a>.</p>"),
    "  <script>",
    paste0("    window.location.replace(\"", relative_target,
           "\" + window.location.search + window.location.hash);"),
    "  </script>",
    "</body>",
    "</html>"
  )
}

write_legacy_redirects <- function(site_dir) {
  canonical_root <- "https://wincowgerdev.github.io/OpenSpecy-package/pkgdown/"
  redirects <- list(
    list("articles/index.html", "../pkgdown/articles/", "articles/", "Articles"),
    list("articles/app.html", "../pkgdown/articles/app.html", "articles/app.html",
         "App tutorial"),
    list("articles/sop.html", "../pkgdown/articles/sop.html", "articles/sop.html",
         "Standard operating procedure"),
    list("articles/advanced.html", "../pkgdown/articles/advanced.html",
         "articles/advanced.html", "Advanced workflows"),
    list("articles/library-builder.html",
         "../pkgdown/articles/library-builder.html",
         "articles/library-builder.html", "Library builder tutorial"),
    list("articles/spectragryph.html", "../pkgdown/articles/spectragryph.html",
         "articles/spectragryph.html", "Spectragryph tutorial"),
    list("reference/index.html", "../pkgdown/reference/", "reference/",
         "R function reference"),
    list("news/index.html", "../pkgdown/news/", "news/", "Release notes"),
    list("authors.html", "pkgdown/authors.html", "authors.html",
         "Authors and citation"),
    list("LICENSE.html", "pkgdown/LICENSE.html", "LICENSE.html", "License")
  )

  for (redirect in redirects) {
    destination <- file.path(site_dir, redirect[[1]])
    dir.create(dirname(destination), recursive = TRUE, showWarnings = FALSE)
    writeLines(
      redirect_document(
        relative_target = redirect[[2]],
        canonical_target = paste0(canonical_root, redirect[[3]]),
        label = redirect[[4]]
      ),
      destination,
      useBytes = TRUE
    )
  }
  invisible(length(redirects))
}

stage_pages_shell <- function(site_dir, repo_root = repo_root_from_script()) {
  repo_root <- normalizePath(repo_root, winslash = "/", mustWork = TRUE)
  source_dir <- file.path(repo_root, "site")
  app_asset_dir <- file.path(repo_root, "inst", "shiny", "www")
  site_dir <- normalize_future_path(site_dir)

  if (!dir.exists(source_dir)) {
    stop("Landing source directory does not exist: ", source_dir,
         call. = FALSE)
  }
  if (path_is_within(site_dir, source_dir) ||
      identical(tolower(site_dir), tolower(repo_root))) {
    stop("The Pages output must not be the repository root or inside site/.",
         call. = FALSE)
  }

  dir.create(site_dir, recursive = TRUE, showWarnings = FALSE)
  if (!dir.exists(site_dir)) {
    stop("Could not create Pages output directory: ", site_dir, call. = FALSE)
  }

  staged_source <- copy_tree(source_dir, site_dir)

  app_assets <- c(
    "logo.png" = "openspecy-logo.png",
    "favicon.png" = "favicon.png",
    "favicon.ico" = "favicon.ico"
  )
  for (source_name in names(app_assets)) {
    source <- file.path(app_asset_dir, source_name)
    if (!file.exists(source)) {
      stop("Missing shared app asset: ", source, call. = FALSE)
    }
    copy_file_checked(
      source,
      file.path(site_dir, "assets", unname(app_assets[[source_name]]))
    )
  }

  nojekyll <- file.path(site_dir, ".nojekyll")
  if (!file.exists(nojekyll) && !file.create(nojekyll)) {
    stop("Could not create ", nojekyll, call. = FALSE)
  }
  redirect_count <- write_legacy_redirects(site_dir)

  cat(
    "Staged Pages landing shell at ", site_dir, " (",
    length(staged_source), " source files, ", length(app_assets),
    " shared assets, ", redirect_count, " compatibility redirects).\n",
    sep = ""
  )
  invisible(site_dir)
}

if (sys.nframe() == 0L) {
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) != 1L || !nzchar(args[[1]])) {
    stop("Usage: Rscript tools/wasm/stage-pages-shell.R <site-output-root>",
         call. = FALSE)
  }
  stage_pages_shell(args[[1]])
}
