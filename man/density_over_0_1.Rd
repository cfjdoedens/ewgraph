% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/density_over_0_1.R
\name{density_over_0_1}
\alias{density_over_0_1}
\title{Create vector of densities over \code{[0, 1]}}
\usage{
density_over_0_1(values_from_0_1, S)
}
\arguments{
\item{values_from_0_1}{Is a vector of values from \code{[0, 1]}}

\item{S}{The number of buckets in which we separate
the elements of the input vector.}
}
\value{
A vector of size \code{S}. Each element i of the vector contains
the count of the values from \code{values_from_01} that have a value in the
range (1/S - 1/2S, 1/S + 1/2S].
}
\description{
Returns a vector of counts of the input.
The count is per i in 1:S,  where the elements of the input in
\code{(i/S - 1/2S, i/S + 1/2S]} are counted in the same bucket.
}
\details{
The values equal 0 are counted in the first bucket.
}
\examples{
    y <- seq(0, 1, length.out=33)
    x <- density_over_0_1(values_from_0_1 = y, S = 12)
}
