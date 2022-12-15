% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/conform_spec.R
\name{conform_spec}
\alias{conform_spec}
\alias{conform_spec.default}
\alias{conform_spec.OpenSpecy}
\title{Adjust spectral intensities to absorbance units}
\usage{
conform_spec(spec, x, xout)

conform_spec.default(object, ...)

conform_spec.OpenSpecy(object, type = "none", make_rel = TRUE, ...)
}
\arguments{
\item{object}{a list object of class \code{OpenSpecy}.}

\item{type}{a character string specifying whether the input spectrum is
in absorbance units (\code{"none"}, default) or needs additional conversion
from \code{"reflectance"} or \code{"transmittance"} data.}

\item{make_rel}{logical; if \code{TRUE} spectra are automatically normalized
with \code{\link{make_rel}()}.}

\item{\ldots}{further arguments passed to submethods; this is
to \code{\link{adj_neg}()} for \code{adj_intens()} and
to \code{\link{conform_res}()} for \code{conform_intens()}.}
}
\value{
\code{adj_intens()} returns a data frame containing two columns
named \code{"wavenumber"} and \code{"intensity"}.
}
\description{
Converts reflectance or transmittance intensity units to absorbance units.
}
\details{
Many of the Open Specy functions will assume that the spectrum is in
absorbance units. For example, see \code{\link{match_spec}()} and
\code{\link{subtr_bg}()}.
To run those functions properly, you will need to first convert any spectra
from transmittance or reflectance to absorbance using this function.
The transmittance adjustment uses the \eqn{log10(1 / T)}
calculation which does not correct for system and particle characteristics.
The reflectance adjustment uses the Kubelka-Munk equation
\eqn{(1 - R)^2 / 2R}. We assume that the reflectance intensity
is a percent from 1-100 and first correct the intensity by dividing by 100
so that it fits the form expected by the equation.
}
\examples{
data("raman_hdpe")

adj_intens(raman_hdpe)

}
\seealso{
\code{\link{subtr_bg}()} for spectral background correction;
\code{\link{match_spec}()} matches spectra with the Open Specy or other
reference libraries
}
\author{
Win Cowger, Zacharias Steinmetz
}