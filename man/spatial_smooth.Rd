% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/spatial_smooth.R
\name{spatial_smooth}
\alias{spatial_smooth}
\title{Spatial Smoothing of OpenSpecy Objects}
\usage{
spatial_smooth(x, sigma = c(1, 1, 1), ...)
}
\arguments{
\item{x}{an \code{OpenSpecy} object.}

\item{sigma}{a numeric vector specifying the standard deviations for the
Gaussian kernel in the x and y dimensions, respectively.}

\item{\dots}{further arguments passed to or from other methods.}
}
\value{
An \code{OpenSpecy} object with smoothed spectra.
}
\description{
Applies spatial smoothing to an \code{OpenSpecy} object using a Gaussian filter.
}
\details{
This function performs spatial smoothing on the spectral data in an \code{OpenSpecy} object.
It assumes that the spatial coordinates are provided in the \code{metadata} element of the object,
specifically in the \code{x} and \code{y} columns, and that there is a \code{col_id} column in \code{metadata} that
matches the column names in the \code{spectra} data.table.
}
\seealso{
\code{\link{as_OpenSpecy}()}, \code{\link[mmand]{gaussianSmooth}()}
}
\author{
Win Cowger
}
