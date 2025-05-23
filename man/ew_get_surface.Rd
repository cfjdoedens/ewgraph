% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{ew_get_surface}
\alias{ew_get_surface}
\title{Get the surface column of an equal width graph.}
\usage{
ew_get_surface(g)
}
\arguments{
\item{g}{The equal width graph}
}
\value{
The surface column as a vector
}
\description{
Get the surface column of an equal width graph.
}
\examples{
  g <- ew_from_vec(c(1, 2, 3))
  ew_get_surface(g)
}
