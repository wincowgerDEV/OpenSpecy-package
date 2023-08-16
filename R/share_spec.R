#' @title Share data with the Open Specy community
#'
#' @description
#' This helper function shares spectral data and metadata with the Open Specy
#' community.
#'
#' \strong{Please note} that \code{share_spec()} only provides basic sharing
#' functionality if used interactively. This means that files are only formatted
#' and saved for sharing but are not sent automatically. This only works with
#' hosted instances of Open Specy.
#'
#' @param x a list object of class \code{OpenSpecy}.
#' @param file File to share (optional).
#' @param share Accepts any local directory to save the spectrum for later
#' sharing via email to \email{wincowger@gmail.com}; \code{"system"} (default)
#' uses the Open Specy package directory at \code{system.file("extdata",
#' package = "OpenSpecy")}; if a correct API token exists, \code{"cloud"}
#' shares the spectrum with the cloud.
#' @param s3_key_id AWS S3 access key ID (required if \code{share = "cloud"}).
#' @param s3_secret_key AWS S3 secret access key (required if \code{share = "cloud"}).
#' @param s3_region AWS S3 region (required if \code{share = "cloud"}).
#' @param s3_bucket AWS S3 bucket name (required if \code{share = "cloud"}).
#' @param ... Further arguments passed to the submethods.
#'
#' @return
#' \code{share_spec()} returns only messages/warnings.
#'
#' @examples
#' \dontrun{
#' data("raman_hdpe")
#' share_spec(raman_hdpe,
#'            metadata = list(
#'              user_name = "Win Cowger",
#'              spectrum_type = "FTIR",
#'              spectrum_identity = "PE",
#'              license = "CC BY-NC"
#'            ),
#'            share = tempdir())
#' }
#'
#' @author
#' Zacharias Steinmetz, Win Cowger
#'
#' @seealso
#' \code{\link{read_text}()};
#' \code{\link[digest]{digest}()}; \code{\link[utils]{sessionInfo}()}
#'
#' @importFrom digest digest
#' @importFrom utils write.csv sessionInfo
#'
#' @export
share_spec <- function(x, ...) {
  UseMethod("share_spec")
}

#' @rdname share_spec
#'
#' @export
share_spec.default <- function(x, ...) {
  stop("object 'x' needs to be of class 'OpenSpecy'", call. = F)
}

#' @rdname share_spec
#'
#' @export
share_spec.OpenSpecy <- function(x, file = NULL, share = "system", s3_key_id = NULL, s3_secret_key = NULL, s3_region = NULL, s3_bucket = NULL,
                                 ...) {
  md <- x$metadata
  if (any(!c("user_name", "spectrum_type", "spectrum_identity") %in%
          names(md)) |
      is.null(md$user_name) | is.null(md$spectrum_type) |
      is.null(md$spectrum_identity))
    warning("fields 'user_name', 'spectrum_type', and 'spectrum_identity' ",
            "should not be empty if you like to share your metadata", call. = F)

  if (share == "system") {
    fp <- file.path(system.file("extdata", package = "OpenSpecy"),
                    "user_spectra", md$session_id)
  } else if (share == "cloud") {
    pkg <- "aws.s3"
    mpkg <- pkg[!(pkg %in% installed.packages()[ , "Package"])]
    if (length(mpkg)) stop("share = 'cloud' requires package 'aws.s3'", call. = F)
    if(any(is.null(s3_key_id), is.null(s3_secret_key), is.null(s3_region), is.null(s3_bucket))) {
      stop("need all s3 inputs to share with the cloud", call. = F)
    }
    Sys.setenv(
      "AWS_ACCESS_KEY_ID" = s3_key_id,
      "AWS_SECRET_ACCESS_KEY" = s3_secret_key,
      "AWS_DEFAULT_REGION" = s3_region
    )

    fp <- file.path(tempdir(), md$session_id)
  } else {
    fp <- file.path(share, md$session_id)
  }
  dir.create(fp, recursive = T, showWarnings = F)

  fd <- file.path(fp, paste0(md$file_id, ".yml"))

  write_spec(x, fd)

  if (!is.null(file)) {
    ex <- strsplit(basename(file), split="\\.")[[1]]
    file.copy(file, file.path(fp, paste0(md$file_id, ".", ex[-1])))
  }

  if (share == "cloud") {
    for (lf in list.files(fp, pattern = md$file_id, full.names = T)) {
      aws.s3::put_object(
        file = lf,
        #object = paste0(hashed_data, ".zip"),
        bucket = s3_bucket
      )
    }
  }

  message("thank you for your willigness to share your data; ",
          "your data has been saved to\n    ",
          fp, "\n",
          "if you run Open Specy locally, you may consider e-mailing your ",
          "files to\n    ",
          "Win Cowger <wincowger@gmail.com>")
}
