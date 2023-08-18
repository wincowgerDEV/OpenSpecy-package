#' @title Share data with the Open Specy community
#'
#' @description
#' This helper function shares spectral data and metadata with the Open Specy
#' community.
#'
#' \strong{Please note:} that \code{share_spec()} only provides basic sharing
#' functionality if used interactively. This means that files are only formatted
#' and saved for sharing but are not sent automatically. This only works with
#' hosted instances of Open Specy.
#'
#' @param x a list object of class \code{OpenSpecy}.
#' @param file file to share (optional).
#' @param share accepts any local directory to save the spectrum for later
#' sharing via email to \email{wincowger@gmail.com}; \code{"system"} (default)
#' uses the Open Specy package directory at \code{system.file("extdata",
#' package = "OpenSpecy")}; if a correct API token exists, \code{"cloud"}
#' shares the spectrum with the cloud.
#' @param credentials a named list of credentials for cloud sharing; required if
#' \code{share = "cloud"}).
#' @param \ldots further arguments passed to the submethods.
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
#'            ))
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
share_spec.OpenSpecy <- function(x, file = NULL, share = "system",
                                 credentials = NULL, ...) {
  md <- x$metadata
  if (any(!c("user_name", "spectrum_type", "spectrum_identity") %in%
          names(md)) |
      is.null(md$user_name) | is.null(md$spectrum_type) |
      is.null(md$spectrum_identity))
    warning("Fields 'user_name', 'spectrum_type', and 'spectrum_identity' ",
            "should not be empty if you like to share your metadata", call. = F)

  if (share == "system") {
    fp <- file.path(system.file("extdata", package = "OpenSpecy"),
                    "user_spectra", md$session_id)
  } else if (share == "cloud") {
    pkg <- "aws.s3"
    mpkg <- pkg[!(pkg %in% installed.packages()[ , "Package"])]
    if(length(mpkg)) stop("share = 'cloud' requires package 'aws.s3'",
                           call. = F)

    if(is.null(credentials))
      stop("'credentials' required to share with the cloud", call. = F)

    if(!is.list(credentials) || !(all(c("s3_key", "s3_secret", "s3_region",
                                        "s3_bucket") %in% names(credentials))))
      stop("'credentials' needs to be a named list containing the following ",
           "items: 's3_key', 's3_secret', 's3_region', 's3_bucket'", call. = F)

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
        bucket = credentials$s3_bucket,
        key = credentials$s3_key, secret = credentials$s3_secret,
        region = credentials$s3_region
      )
    }
  }

  message("Thank you for your willigness to share your data; ",
          "your data has been saved to\n    ",
          fp, "\n",
          "If you run Open Specy locally, you may consider e-mailing your ",
          "files to\n    ",
          "Win Cowger <wincowger@gmail.com>")
}
