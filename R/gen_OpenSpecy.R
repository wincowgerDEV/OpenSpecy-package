#' @title Generic Open Specy Functions
#' @rdname gen_OpenSpecy
#'
#' @description
#' Functions to manipulate and visualize OpenSpecy objects.
#'
#' @details
#' \code{as_OpenSpecy()} creates an OpenSpecy object.
#' \code{print.OpenSpecy()} prints the contents of an OpenSpecy object.
#' \code{head.OpenSpecy()} shows the first few lines of an OpenSpecy object.
#' \code{plot.OpenSpecy()} visualizes spectral data of an OpenSpecy object.
#' \code{sample.OpenSpecy()} samples spectra from an OpenSpecy object.
#'
#' @param x an OpenSpecy object.
#' @param \ldots args.
#' @param size a positive integer specifying the number of items to select from the spectra. (only for \code{sample.OpenSpecy()})
#' @param replace should sampling be done with replacement? (only for \code{sample.OpenSpecy()})
#' @param prob a vector of probability weights for obtaining the elements of the spectra being sampled. (only for \code{sample.OpenSpecy()})
#'
#' @return
#' \code{print.OpenSpecy()} returns a textual representation of an OpenSpecy object.
#' \code{head.OpenSpecy()} returns the first few lines of an OpenSpecy object.
#' \code{plot.OpenSpecy()} returns a ggplot object representing the spectral data.
#' \code{sample.OpenSpecy()} returns an OpenSpecy object with a subset of the spectra.
#'
#' @examples
#'
#' # Creating an OpenSpecy object
#' sample_OpenSpecy <- read_any(read_extdata("raman_hdpe.csv"))
#'
#' # Printing the OpenSpecy object
#' print.OpenSpecy(sample_OpenSpecy)
#'
#' # Displaying the first few lines of the OpenSpecy object
#' head(sample_OpenSpecy)
#'
#' # Plotting the OpenSpecy object
#' plot.OpenSpecy(sample_OpenSpecy)
#'
#' # Sampling the OpenSpecy object
#' sample_OpenSpecy <- read_any(read_extdata("CA_tiny_map.zip"))
#' sampled_OpenSpecy <- sample.OpenSpecy(sample_OpenSpecy, size = 5)
#' print.OpenSpecy(sampled_OpenSpecy)
#' plot.OpenSpecy(sampled_OpenSpecy)
#'
#' @author
#' Zacharias Steinmetz, Win Cowger
#'
#' @seealso
#' Other related functions.
#'
#' @importFrom magrittr %>%
#' @importFrom utils head
#' @importFrom tidyr pivot_longer
#' @importFrom ggplot2 ggplot aes geom_line theme_minimal labs
#' @export
head.OpenSpecy <- function(x, ...) {
  cbind(wavenumber = x$wavenumber, x$spectra) %>% head(...)
}

#' @rdname gen_OpenSpecy
#'
#' @export
print.OpenSpecy <- function(x, ...) {
  cbind(wavenumber = x$wavenumber, x$spectra) %>% print(...)
  cat("\n$metadata\n")
  print(x$metadata)
}


#' @rdname gen_OpenSpecy
#' 
#' @export
sample.OpenSpecy <- function(x, size, prob = NULL, ...) {
    
    #Replace = false is mandatory currently because we don't have a way to rename and recoordinate duplicates. 
    spectra_to_sample <- sample(1:ncol(x$spectra), size = size, replace = FALSE, prob = prob)
    
    as_OpenSpecy(
        x = x$wavenumber, 
        spectra = x$spectra[,..spectra_to_sample],
        metadata = x$metadata[spectra_to_sample,]
    )
}


#Could be nice to add at some point.
#summary.OpenSpecy <- function(x, ...) {
    
#}
