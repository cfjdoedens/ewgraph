% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{ew_round_prob}
\alias{ew_round_prob}
\title{Round probability value in accordance with number of segments of equal width graph}
\usage{
ew_round_prob(p, S)
}
\arguments{
\item{p}{The probability to round}

\item{S}{The number of segments of teh equal width graph}
}
\value{
The rounded value
}
\description{
This function needs some more thought.
It now returns a string.
It is probably better if it returns a number.
It needs also better explaining.
}
\examples{
  ew_round_prob(0.0356, 10)
}
