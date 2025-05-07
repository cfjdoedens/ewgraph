#' Create vector of densities over \code{[0, 1]}
#'
#' Returns a vector of counts of the input.
#' The count is per i in 1:S,  where the elements of the input in
#' \code{(i/S - 1/2S, i/S + 1/2S]} are counted in the same bucket.
#'
#' The values equal 0 are counted in the first bucket.
#'
#' @param values_from_0_1 Is a vector of values from \code{[0, 1]}
#' @param S The number of buckets in which we separate
#'     the elements of the input vector.
#' @returns A vector of size `S`. Each element i of the vector contains
#'          the count of the values from `values_from_01` that have a value in the
#'          range (1/S - 1/2S, 1/S + 1/2S].
#' examples
#'     y <- seq(0, 1, length.out=33)
#'     x <- density_over_0_1(values_from_0_1 = y, S = 12)
density_over_0_1 <- function(values_from_0_1, S) {
  # Check input.
  stopifnot(all(is.numeric(values_from_0_1)))
  stopifnot(all(0 <= values_from_0_1))
  stopifnot(all(values_from_0_1 <= 1))
  stopifnot(posint(S))

  # Create vector to be returned.
  r <- numeric(S)

  # Initial fill of the buckets in the vector.
  for (i in seq_along(values_from_0_1)) {
    bucket_nr <- ceiling(S * values_from_0_1[[i]])
    if (bucket_nr == 0) {
      bucket_nr <- 1
    }
    r[[bucket_nr]] <- r[[bucket_nr]] + 1
  }

  r
}
