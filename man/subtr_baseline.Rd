% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/subtr_baseline.R
\name{subtr_baseline}
\alias{subtr_baseline}
\alias{subtr_baseline.default}
\alias{subtr_baseline.OpenSpecy}
\title{Automated background subtraction for spectral data}
\usage{
subtr_baseline(x, ...)

\method{subtr_baseline}{default}(x, ...)

\method{subtr_baseline}{OpenSpecy}(
  x,
  type = "polynomial",
  degree = 8,
  raw = FALSE,
  baseline,
  make_rel = TRUE,
  ...
)
}
\arguments{
\item{x}{a list object of class \code{OpenSpecy}.}

\item{type}{one of \code{"polynomial"} or \code{"manual"} depending on
whether you want spectra to be corrected with a manual baseline or with
polynomial baseline fitting.}

\item{degree}{the degree of the polynomial. Must be less than the number of
unique points when raw is \code{FALSE}. Typically a good fit can be
found with a 8th order polynomial.}

\item{raw}{if \code{TRUE}, use raw and not orthogonal polynomials.}

\item{baseline}{an \code{OpenSpecy} object containing the baseline data to be
subtracted.}

\item{make_rel}{logical; if \code{TRUE} spectra are automatically normalized
with \code{\link{make_rel}()}.}

\item{\ldots}{further arguments passed to \code{\link[stats]{poly}()}.}
}
\value{
\code{subtr_baseline()} returns a data frame containing two columns named
\code{"wavenumber"} and \code{"intensity"}.
}
\description{
This baseline correction routine iteratively finds the baseline of a spectrum
using a polynomial fitting or accepts a manual baseline.
}
\details{
This is a translation of Michael Stephen Chen's MATLAB code written for the
\code{imodpolyfit} routine from Zhao et al. 2007.
}
\examples{
data("raman_hdpe")

subtr_baseline(raman_hdpe)

}
\references{
Chen MS (2020). Michaelstchen/ModPolyFit. \emph{MATLAB}.
Retrieved from \url{https://github.com/michaelstchen/modPolyFit}
(Original work published July 28, 2015)

Zhao J, Lui H, McLean DI, Zeng H (2007). “Automated Autofluorescence
Background Subtraction Algorithm for Biomedical Raman Spectroscopy.”
\emph{Applied Spectroscopy}, \strong{61}(11), 1225–1232.
\doi{10.1366/000370207782597003}.
}
\seealso{
\code{\link[stats]{poly}()};
\code{\link{smooth_intens}()}
}
\author{
Win Cowger, Zacharias Steinmetz
}
