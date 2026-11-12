#' @rdname manage_lib
#' @title Manage spectral libraries
#'
#' @description
#' These functions will import the spectral libraries from Open Specy if they
#' were not already downloaded. The CRAN does not allow for deployment of large
#' datasets so this was a workaround that we are using to make sure everyone can
#' easily get Open Specy functionality running on their desktop. Please see the
#' references when using these libraries. These libraries are the accumulation
#' of a massive amount of effort from independant groups and each should be
#' attributed when you are using their data.
#'
#' @details
#' \code{check_lib()} checks to see if the Open Specy reference library
#' already exists on the users computer.
#' \code{get_lib()} downloads the Open Specy library from its AWS distribution.
#' \code{load_lib()} will load the library into the global environment for use
#' with the Open Specy functions.
#' \code{rm_lib()} removes the libraries from your computer.
#'
#' @param type library type to check/retrieve; defaults to
#' \code{c("derivative", "nobaseline", "raw", "medoid_derivative", "medoid_nobaseline", "model_derivative", "model_nobaseline")} which reads
#' everything.
#' @param path where to save or look for local library files; defaults to
#' \code{"system"} pointing to
#' \code{system.file("extdata", package = "OpenSpecy")}.
#' @param mode see \code{?download.file} for details on mode.
#' @param revision optional AWS S3 \code{versionId}. A single value applies to
#' every requested type; a named character vector can pin each type separately.
#' If \code{NULL}, the current unversioned object is downloaded.
#' @param date optional target library date as one \code{Date} or strict
#' \code{"YYYY-MM-DD"} value. For each requested type, \code{get_lib()} uses
#' the catalogued version with the smallest calendar-date difference, preferring
#' the earlier version on an exact tie. The current AWS object is discovered
#' live and included among the candidates. \code{date} and \code{revision}
#' cannot be supplied together.
#' @param condition determines if \code{check_lib()} should warn
#' (\code{"warning"}, the default) or throw and error (\code{"error"}).
#' @param \ldots further arguments passed to \code{\link[utils]{download.file}()}.
#'
#' @return
#' \code{check_lib()} and \code{get_lib()} return messages only;
#' \code{load_lib()} returns an \code{OpenSpecy} object containing the
#' respective spectral reference library.
#'
#' @examples
#' \dontrun{
#' #check to see if you have the library already
#' check_lib("derivative")
#' 
#' #get the library stored in your system from online repo
#' get_lib("derivative")
#'
#' #get the catalogued release closest to a target date
#' get_lib("derivative", date = "2026-10-01")
#' 
#' #load the library into the working environment
#' spec_lib <- load_lib("derivative")
#' 
#' #for models you should choose either both, ftir, or raman as a list item before use
#' get_lib("model_derivative")
#' mod_lib <- load_lib("model_derivative")[["ftir"]]
#' 
#' }
#'
#' @author
#' Zacharias Steinmetz, Win Cowger
#'
#' @references
#' Bell IB, Clark RJH, Gibbs PJ (2010). “Raman Spectroscopic Library.”
#' *Christopher Ingold Laboratories, University College London, UK*.
#'
#' Berzinš K, Sales RE, Barnsley JE, Walker G, Fraser-Miller SJ, Gordon KC
#' (2020). “Low-Wavenumber Raman Spectral Database of Pharmaceutical
#' Excipients.” *Vibrational Spectroscopy* **107**, 103021.
#' \doi{10.5281/zenodo.3614035}.
#'
#' Cabernard L, Roscher L, Lorenz C, Gerdts G, Primpke S (2018). “Comparison of
#' Raman and Fourier Transform Infrared Spectroscopy for the Quantification of
#' Microplastics in the Aquatic Environment.” *Environmental Science &
#' Technology* **52**(22), 13279--13288. \doi{10.1021/acs.est.8b03438}.
#'
#' Caggiani MC, Cosentino A, Mangone A (2016). “Pigments Checker version 3.0, a
#' handy set for conservation scientists: A free online Raman spectra database.”
#' *Microchemical Journal* **129**, 123--132.
#' \doi{10.1016/j.microc.2016.06.020}.
#'
#' Chabuka BK, Kalivas JH (2020). “Application of a Hybrid Fusion Classification
#' Process for Identification of Microplastics Based on Fourier Transform
#' Infrared Spectroscopy. Applied Spectroscopy.” *Applied Spectroscopy*
#' **74**(9), 1167--1183. \doi{10.1177/0003702820923993}.
#'
#' Cowger, W (2023). “Library data.” \emph{OSF}. \doi{10.17605/OSF.IO/X7DPZ}.
#'
#' Cowger W, Gray A, Christiansen SH, De Frond H, Deshpande AD, Hemabessiere L,
#' Lee E, Mill L, et al. (2020). “Critical Review of Processing and
#' Classification Techniques for Images and Spectra in Microplastic Research.”
#' \emph{Applied Spectroscopy}, \strong{74}(9), 989--1010.
#' \doi{10.1177/0003702820929064}.
#'
#' Cowger W, Roscher L, Chamas A, Maurer B, Gehrke L, Jebens H, Gerdts G,
#' Primpke S (2023). “High Throughput FTIR Analysis of Macro and Microplastics
#' with Plate Readers.” *ChemRxiv Preprint*. \doi{10.26434/chemrxiv-2023-x88ss}.
#'
#' De Frond H, Rubinovitz R, Rochman CM (2021). “µATR-FTIR Spectral Libraries of
#' Plastic Particles (FLOPP and FLOPP-e) for the Analysis of Microplastics.”
#' *Analytical Chemistry* **93**(48), 15878--15885.
#' \doi{10.1021/acs.analchem.1c02549}.
#'
#' El Mendili Y, Vaitkus A, Merkys A, Gražulis S, Chateigner D, Mathevet F,
#' Gascoin S, Petit S, Bardeau JF, Zanatta M, Secchi M, Mariotto G, Kumar A,
#' Cassetta M, Lutterotti L, Borovin E, Orberger B, Simon P, Hehlen B,
#' Le Guen M (2019). “Raman Open Database: first interconnected Raman–X-ray
#' diffraction open-access resource for material identification.” *Journal of
#' Applied Crystallography*, **52**(3), 618--625.
#' \doi{10.1107/s1600576719004229}.
#'
#' Johnson TJ, Blake TA, Brauer CS, Su YF, Bernacki BE, Myers TL, Tonkyn RG,
#' Kunkel BM, Ertel AB (2015). “Reflectance Spectroscopy for Sample
#' Identification: Considerations for Quantitative Library
#' Results at Infrared Wavelengths.” *International Conference on Advanced
#' Vibrational Spectroscopy (ICAVS 8)*.
#' \url{https://www.osti.gov/biblio/1452877}.
#'
#' Lafuente R, Downs RT, Yang H, Stone N (2016). “The power of databases: The
#' RRUFF project.” *Highlights in Mineralogical Crystallography*.
#' \doi{10.1515/9783110417104-003}.
#'
#' Munno K, De Frond H, O’Donnell B, Rochman CM (2020). “Increasing the
#' Accessibility for Characterizing Microplastics: Introducing New
#' Application-Based and Spectral Libraries of Plastic Particles (SLoPP and
#' SLoPP-E).” *Analytical Chemistry* **92**(3), 2443--2451.
#' \doi{10.1021/acs.analchem.9b03626}.
#'
#' Myers TL, Brauer CS, Su YF, Blake TA, Johnson TJ, Richardson RL (2014).
#' “The influence of particle size on infrared reflectance spectra.”
#' *Proceedings Volume 9088, Algorithms and Technologies for Multispectral,
#' Hyperspectral, and Ultraspectral Imagery XX*, 908809.
#' \doi{10.1117/12.2053350}.
#'
#' Myers TL, Brauer CS, Su YF, Blake TA, Tonkyn RG, Ertel AB, Johnson TJ,
#' Richardson RL (2015). “Quantitative reflectance spectra of solid powders
#' as a function of particle size.” *Applied Optics* **54**(15), 4863--4875.
#' \doi{10.1364/ao.54.004863}.
#'
#' Primpke S, Wirth M, Lorenz C, Gerdts G (2018). “Reference database design
#' for the automated analysis of microplastic samples based on Fourier
#' transform infrared (FTIR) spectroscopy.”
#' *Analytical and Bioanalytical Chemistry* **410**, 5131–-5141.
#' \doi{10.1007/s00216-018-1156-x}.
#'
#' Roscher L, Fehres A, Reisel L, Halbach M, Scholz-Böttcher B, Gerriets M,
#' Badewien TH, Shiravani G, Wurpts A, Primpke S, Gerdts G (2021). “Abundances
#' of large microplastics (L-MP, 500-5000 µm) in surface waters of the Weser
#' estuary and the German North Sea.” *PANGAEA*. \doi{10.1594/PANGAEA.938143}.
#'
#' “Handbook of Raman Spectra for geology” (2023).
#'
#' “Scientific Workgroup for the Analysis of Seized Drugs.” (2023).
#' https://swgdrug.org/ir.htm.
#'
#' **Further contribution of spectra:**
#' Suja Sukumaran (Thermo Fisher Scientific),
#' Aline Carvalho,
#' Jennifer Lynch (NIST),
#' Claudia Cella and Dora Mehn (JRC),
#' Horiba Scientific,
#' USDA Soil Characterization Data,
#' Archaeometrielabor,
#' and S.B. Engelsen (Royal Vet. and Agricultural University, Denmark).
#' Kimmel Center data was collected and provided by Prof. Steven Weiner
#' (Kimmel Center for Archaeological Science, Weizmann Institute of Science,
#' Israel).
#'
#' @export
check_lib <- function(type = c("derivative", "nobaseline", "raw", "medoid_derivative",
                               "medoid_nobaseline", "model_derivative", "model_nobaseline"),
                      path = "system", condition = "warning") {

  lp <- ifelse(path == "system",
               system.file("extdata", package = "OpenSpecy"),
               path)

  .chkf(type, path = lp, condition = condition)

  invisible()
}

# Internal catalog for the current OpenSpecy reference-library objects. The
# unversioned CloudFront URLs always resolve to the latest maintainer upload;
# callers can still pass an explicit S3 versionId to get_lib() for historical
# comparisons or reproducible tests.
.openspecy_library_catalog <- function() {
  data.frame(
    type = c(
      "derivative", "nobaseline", "raw", "medoid_derivative",
      "medoid_nobaseline", "model_derivative", "model_nobaseline"
    ),
    filename = c(
      "derivative.rds", "nobaseline.rds", "raw.rds",
      "medoid_derivative.rds", "medoid_nobaseline.rds",
      "model_derivative.rds", "model_nobaseline.rds"
    ),
    stringsAsFactors = FALSE
  )
}

# Historical versions remain package-owned because the CloudFront distribution
# permits exact object reads but its private S3 origin does not expose version
# listings. The live current version is appended by .library_versions_for().
.openspecy_library_versions <- function() {
  data.frame(
    type = rep(
      c(
        "derivative", "nobaseline", "raw", "medoid_derivative",
        "medoid_nobaseline", "model_derivative", "model_nobaseline"
      ),
      each = 2L
    ),
    date = as.Date(rep(c("2026-09-22", "2026-10-21"), 7L)),
    version_id = c(
      "plEFh7vekJt_2rcBqEud6KeNTU07FfUy",
      "PYhevbuo1qsTfvGyV8952X6oOk863L5k",
      "6rVVsE1G9TaCoAsAa21FZaNNjgXsmhwj",
      "lG.bJ00N5WkUCNUlEmKNQ8oBoxzyGe7n",
      "ysLLpg5ORhA2sWl6R0hnhaV7CfROPOEU",
      "LMOcxnkXhqXG_vVdXLY33smJXI9jtMK1",
      "qWwA0_NJo59R38afYgI7vo7Js.paPDWA",
      "lw5qXSqPAV2njxPkMUmfJGqJJzIOaj58",
      "t7lzVXWzVVVIuLn9uce.OQrnx79aHJD0",
      ".h0XGZJ8K9j2tkIT_qaD3Lw7kHhRR9v2",
      "ExI.nMpxauEqSgXLr48yO.teTD9NemHz",
      "MjpnerGc_ECQL0NWuHW5XTcls1f9ceNp",
      "WnkXuBzPHSC7GKrG.bgz2ztqtszfLRc2",
      "vfJc3OWlBKEhZIFNifzggvIZegSX44F1"
    ),
    stringsAsFactors = FALSE
  )
}

.library_target_date <- function(date) {
  if (is.null(date)) return(NULL)

  if (inherits(date, "Date")) {
    if (length(date) != 1L || is.na(date)) {
      stop("'date' must contain one non-missing calendar date", call. = FALSE)
    }
    return(date)
  }

  if (!is.character(date) || length(date) != 1L || is.na(date) ||
      !grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", date)) {
    stop("'date' must be one Date or a 'YYYY-MM-DD' string", call. = FALSE)
  }

  parsed <- suppressWarnings(as.Date(date, format = "%Y-%m-%d"))
  if (is.na(parsed) || !identical(format(parsed, "%Y-%m-%d"), date)) {
    stop("'date' must be a valid calendar date in 'YYYY-MM-DD' format",
         call. = FALSE)
  }
  parsed
}

.library_http_date <- function(value) {
  match <- regexec(
    "^[A-Za-z]{3}, ([0-9]{2}) ([A-Za-z]{3}) ([0-9]{4}) ", value
  )
  parts <- regmatches(value, match)[[1L]]
  months <- setNames(sprintf("%02d", seq_len(12L)), month.abb)
  if (length(parts) != 4L || !parts[[3L]] %in% names(months)) {
    stop("AWS returned an invalid Last-Modified library date", call. = FALSE)
  }
  parsed <- as.Date(paste(parts[[4L]], months[[parts[[3L]]]], parts[[2L]],
                          sep = "-"))
  if (is.na(parsed)) {
    stop("AWS returned an invalid Last-Modified library date", call. = FALSE)
  }
  parsed
}

.library_header_value <- function(headers, name) {
  values <- grep(
    paste0("^", name, "[[:space:]]*:"), headers,
    ignore.case = TRUE, value = TRUE
  )
  if (!length(values)) return(NULL)
  trimws(sub("^[^:]+:", "", tail(values, 1L)))
}

.library_remote_metadata <- function(url) {
  headers <- curlGetHeaders(url, timeout = 30L)
  status <- attr(headers, "status")
  if (length(status) != 1L || is.na(status) || status < 200L || status >= 300L) {
    stop("AWS did not return library release metadata for '", url, "'",
         call. = FALSE)
  }

  modified <- .library_header_value(headers, "Last-Modified")
  version_id <- .library_header_value(headers, "x-amz-version-id")
  if (is.null(modified) || is.null(version_id) || !nzchar(version_id)) {
    stop("AWS library metadata is missing Last-Modified or x-amz-version-id",
         call. = FALSE)
  }

  list(date = .library_http_date(modified), version_id = version_id)
}

.library_versions_for <- function(type, current) {
  versions <- .openspecy_library_versions()
  versions <- versions[versions$type == type, , drop = FALSE]
  current_row <- data.frame(
    type = type, date = current$date, version_id = current$version_id,
    stringsAsFactors = FALSE
  )
  versions <- rbind(versions, current_row)
  versions <- versions[!duplicated(versions$version_id, fromLast = TRUE), ,
                       drop = FALSE]
  versions$is_current <- versions$version_id == current$version_id
  rownames(versions) <- NULL
  versions
}

.library_version_nearest <- function(type, target, current) {
  versions <- .library_versions_for(type, current)
  if (!nrow(versions)) {
    stop("No dated library versions are catalogued for '", type, "'",
         call. = FALSE)
  }

  distance <- abs(as.integer(versions$date - target))
  # Earlier calendar dates win ties; for two versions on that same date, the
  # live current object wins so a same-day replacement remains targetable.
  selected <- order(distance, versions$date, !versions$is_current)[[1L]]
  versions[selected, , drop = FALSE]
}

.library_revision <- function(revision, type) {
  if (is.null(revision)) return(NULL)
  if (!is.character(revision) || anyNA(revision) ||
      any(!nzchar(revision))) {
    stop("'revision' must be NULL or a nonempty character vector",
         call. = FALSE)
  }
  if (length(revision) == 1L) return(unname(revision))
  if (is.null(names(revision)) || !type %in% names(revision)) {
    stop("a multi-value 'revision' must be named for every requested type",
         call. = FALSE)
  }
  unname(revision[[type]])
}

#' @rdname manage_lib
#'
#' @importFrom utils read.csv download.file sessionInfo
#'
#' @export

get_lib <- function(type = c("derivative", "nobaseline", "raw", "medoid_derivative",
                             "medoid_nobaseline", "model_derivative", "model_nobaseline"),
                    path = "system",
                    mode = "wb",
                    revision = NULL,
                    date = NULL,
                    ...) {
  if (!is.null(revision) && !is.null(date)) {
    stop("supply only one of 'revision' or 'date'", call. = FALSE)
  }
  target_date <- .library_target_date(date)
  lp <- ifelse(path == "system",
               system.file("extdata", package = "OpenSpecy"),
               path)
  if (!dir.exists(lp)) dir.create(lp, recursive = TRUE, showWarnings = FALSE)

  catalog <- .openspecy_library_catalog()
  message("Fetching Open Specy reference libraries from AWS ...")

  for (t in type) {
    row <- catalog[catalog$type == t, , drop = FALSE]
    if (!nrow(row)) {
      warning("Unknown library type: ", t)
      next
    }

    label <- gsub("_", " ", t)
    version_id <- .library_revision(revision, t)
    url <- paste0("https://d2jrxerjcsjhs7.cloudfront.net/", row$filename)
    if (!is.null(target_date)) {
      current <- .library_remote_metadata(url)
      selected <- .library_version_nearest(t, target_date, current)
      version_id <- selected$version_id[[1L]]
      metadata <- list(date = selected$date[[1L]], version_id = version_id)
      message(
        "Selected ", label, " library dated ", metadata$date,
        " for requested date ", target_date, "."
      )
    } else {
      metadata_url <- url
      if (!is.null(version_id)) {
        metadata_url <- paste0(
          metadata_url, "?versionId=",
          utils::URLencode(version_id, reserved = TRUE)
        )
      }
      metadata <- .library_remote_metadata(metadata_url)
      if (!is.null(version_id) &&
          !identical(metadata$version_id, version_id)) {
        stop(
          "AWS returned version '", metadata$version_id,
          "' instead of requested revision '", version_id, "'",
          call. = FALSE
        )
      }
    }
    if (!is.null(version_id)) {
      url <- paste0(url, "?versionId=",
                    utils::URLencode(version_id, reserved = TRUE))
    }
    message("Fetching ", label, " library...")
    destfile <- file.path(lp, row$filename)
    download.file(url, destfile = destfile, mode = mode, ...)
    message(
      "Downloaded ", label, " library dated ", metadata$date,
      " (AWS version ", metadata$version_id, ")."
    )

  }

  message("Use 'load_lib()' to load the library")
  invisible()
}

#' @rdname manage_lib
#'
#' @export
load_lib <- function(type, path = "system") {
  lp <- ifelse(path == "system",
               system.file("extdata", package = "OpenSpecy"),
               path)

  chk <- .chkf(type, path = lp, condition = "stop")

  fp <- file.path(lp, paste0(type, ".rds"))

  rds <- readRDS(fp)

  return(rds)
}

#' @rdname manage_lib
#'
#' @export
rm_lib <- function(type = c("derivative", "nobaseline", "raw", "medoid_derivative",
                            "medoid_nobaseline", "model_derivative", "model_nobaseline"),
                   path = "system") {
  lp <- ifelse(path == "system",
               system.file("extdata", package = "OpenSpecy"),
               path)

  fp <- file.path(lp, paste0(type, ".rds"))
  file.remove(fp)

  invisible()
}

# Auxiliary function for library checks
.chkf <- function(type, path = "system", condition = "warning") {
  fn <- paste0(type, ".rds")

  lp <- ifelse(path == "system", system.file("extdata", package = "OpenSpecy"),
               path)

  chk <- file.path(lp, fn) |> file.exists()

  names(chk) <- type

  out <- paste(type[!chk], collapse = ", ")

  if (!all(chk))
    do.call(condition, list("Library missing or incomplete: ", out, "; ",
                            "use 'get_lib()' to download a current version",
                            call. =  ifelse(condition %in%
                                              c("message",
                                                "packageStartupMessage"),
                                            "", FALSE)))
  chk
}
