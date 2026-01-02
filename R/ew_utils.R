# Utility functions for equal width graphs

#' Partition line piece on the real line into equally sized consecutive segments
#'
#' Each segment is represented by its midpoint.
#'
#' @param begin The start of the line piece.
#' @param end The end of the line piece.
#' @param S An integer >= 1. The number of segments to partition into.
#' @returns The vector of segment midpoints.
#' @export
#' @examples
#'   # Returns the vector c(0.1, 0.3, 0.5, 0.7, 0.9).
#'   partition(S = 5)
partition <- function(begin = 0,
                      end = 1,
                      S = 1000) {
  stopifnot(begin < end)
  stopifnot(posint(S))
  r <- numeric(S)
  range <- end - begin
  step <- range / S
  for (i in 1:S) {
    r[[i]] <- begin + step / 2 + (i - 1) * step
  }
  r
}

#' Partition \code{[0, 1]} into equally sized consecutive segments
#'
#' Each segment is represented by its midpoint.
#'
#' @param S An integer >= 1. The number of segments to partition into.
#' @returns The vector of segment midpoints.
#' @export
#' @examples
#'   # Returns the vector c(0.1, 0.3, 0.5, 0.7, 0.9).
#'   partition_0_1(S = 5)
partition_0_1 <- function(S = 1000) {
  stopifnot(posint(S))
  seq(0 + 1 / (2 * S), 1 - 1 / (2 * S), 1 / S)
}

#' Get the number of segments, i.e. number of rows, of an equal width graph.
#'
#' @param g The equal width graph
#'
#' @returns The number of segments, this is, rows of g
#' @export
#'
#' @examples
#'   g <- ew_from_vec(c(1, 2, 3))
#'   ew_S(g)
ew_S <- function(g) {
  length(ew_get_h(g))
}

#' Get the p column of an equal width graph.
#'
#' @param g The equal width graph
#'
#' @returns The p column as a vector
#' @export
#'
#' @examples
#'   g <- ew_from_vec(c(1, 2, 3))
#'   ew_get_p(g)
#' @importFrom dplyr pull
#' @importFrom dplyr %>%
ew_get_p <- function(g) {
  g %>% pull("p")
}

#' Get the h column of an equal width graph.
#'
#' @param g The equal width graph
#'
#' @returns The h column as a vector
#' @export
#'
#' @examples
#'   g <- ew_from_vec(c(1, 2, 3))
#'   ew_get_h(g)
#' @importFrom dplyr pull
#' @importFrom dplyr %>%
ew_get_h <- function(g) {
  g %>% pull("h")
}

#' Get the h_left column of an equal width graph.
#'
#' @param g The equal width graph
#'
#' @returns The h_left column as a vector
#' @export
#'
#' @examples
#'   g <- ew_from_vec(c(1, 2, 3))
#'   ew_get_h_left(g)
#' @importFrom dplyr pull
#' @importFrom dplyr %>%
ew_get_h_left <- function(g) {
  g %>% pull("h_left")
}

#' Get the h_right column of an equal width graph.
#'
#' @param g The equal width graph
#'
#' @returns The h_right column as a vector
#' @export
#'
#' @examples
#'   g <- ew_from_vec(c(1, 2, 3))
#'   ew_get_h_right(g)
#' @importFrom dplyr pull
#' @importFrom dplyr %>%
ew_get_h_right <- function(g) {
  g %>% pull("h_right")
}

#' Get the surface column of an equal width graph.
#'
#' @param g The equal width graph
#'
#' @returns The surface column as a vector
#' @export
#'
#' @examples
#'   g <- ew_from_vec(c(1, 2, 3))
#'   ew_get_surface(g)
#' @importFrom dplyr pull
#' @importFrom dplyr %>%
ew_get_surface <- function(g) {
  g %>% pull("surface")
}

#' Get the cumulative surface column of an equal width graph.
#'
#' @param g The equal width graph
#'
#' @returns The cumulative surface column as a vector
#' @export
#'
#' @examples
#'   g <- ew_from_vec(c(1, 2, 3))
#'   ew_get_cumsurface(g)
#' @importFrom dplyr pull
#' @importFrom dplyr %>%
ew_get_cumsurface <- function(g) {
  g %>% pull("cumsurface")
}

#' Check whether object is a valid equal width graph
#'
#' @param g The object to be checked
#' @param verbose TRUE or FALSE
#'
#' @returns TRUE or FALSE
#'   When g is invalid as an equal width graph
#'   and verbose == TRUE, info why g is invalid
#'   is printed to the console.
#' @export
#'
#' @examples
#'   g <- ew_validate(c(1, 2, 3))
#'   ew_validate(g)
#' @importFrom tibble is_tibble
ew_validate <- function(g, verbose = TRUE) {
  # When we have to do with a list of (supposed) ew_probability_graphs,
  # we call ew_validate() recursively.
  if (class(g)[[1]] == "list") {
    r <- sapply(g, ew_validate)
    return(min(r) > 0) # Only TRUE if all values in r are TRUE
  }

  # Single (supposed) ew_probability_graph to validate.
  # g should be a tibble.
  if (!is_tibble(g)) {
    if (verbose) {
      message("ew_probability_graph should be a tibble")
    }
    return(FALSE)
  }

  # g should have columns p, h, h_left, h_right, surface, cumsurface.
  if (!("p" %in% colnames(g))) {
    if (verbose) {
      message("ew_probability_graph should contain a vector p")
    }
    return(FALSE)
  }
  if (!("h" %in% colnames(g))) {
    if (verbose) {
      message("ew_probability_graph should contain a vector h")
    }
    return(FALSE)
  }
  if (!("h_left" %in% colnames(g))) {
    if (verbose) {
      message("ew_probability_graph should contain a vector h_left")
    }
    return(FALSE)
  }
  if (!("h_right" %in% colnames(g))) {
    if (verbose) {
      message("ew_probability_graph should contain a vector h_right")
    }
    return(FALSE)
  }
  if (!("surface" %in% colnames(g))) {
    if (verbose) {
      message("ew_probability_graph should contain a vector surface")
    }
    return(FALSE)
  }
  if (!("cumsurface" %in% colnames(g))) {
    if (verbose) {
      message("ew_probability_graph should contain a vector cumsurface")
    }
    return(FALSE)
  }

  # g should contain at least 1 segment.
  S <- ew_S(g)
  if (!(S > 0)) {
    if (verbose) {
      message("ew_probability_graph should contain at least one segment")
    }
    return(FALSE)
  }

  # p should be equal to partition(S), where S is the number of segments of g.
  p <- ew_get_p(g)
  p2 <- partition_0_1(S)
  if (!isTRUE(all.equal(p, p2))) {
    if (verbose) {
      message("vector p of ew_probability_graph should be equal to
               partition_0_1(ew_S(g))")
    }
    return(FALSE)
  }

  # The total area of the combined trapeziums should be 1.
  cumsurface <- g %>% pull("cumsurface")
  if (!near(cumsurface[[S]], 1)) {
    if (verbose) {
      message("total chance mass of ew_probability_graph should be 1")
      message(sprintf("found chance mass =  %1.5f", cumsurface[[S]]))
      message(sprintf("number of segments in the chance graph = %i", S))
    }
    return(FALSE)
  }

  return(TRUE)
}

#' One argument constant function that returns 1
#'
#' The domain of this function is the numeric values. Other input
#' values will result in an error.
#'
#' @param x Should be numeric.
#'
#' @returns 1 for x being numeric, otherwise an error.
#' @export
#'
#' @examples
#'   # Returns 1.
#'   unity(-0.034)
#'
#'   # Returns 1.
#'   unity(rnorm(1, mean = 10^10, sd = 10^20))
#'
#'   # Returns 1.
#'   unity(NaN)
#'
#'   # Returns 1.
#'   unity(Inf)
#'
#'   # Returns c(1, 1).
#'   unity(c(3, 7))
#'
#'   # An error is generated for non-numeric input.
#'   tryCatch({
#'     unity("a")
#'   }, error = function(e) {
#'     cat('unity("a") procuded the following error message:', conditionMessage(e), "\n")
#'     NA
#'   })
unity <- function(x) {
  stopifnot(is.numeric(x))
  rep.int(1, length(x))
}

#' Round probability value in accordance with number of segments of equal width graph
#'
#' This function needs some more thought.
#' It now returns a string.
#' It is probably better if it returns a number.
#' It needs also better explaining.
#'
#' @param p The probability to round
#' @param S The number of segments of teh equal width graph
#'
#' @returns The rounded value
#' @export
#'
#' @examples
#'   ew_round_prob(0.0356, 10)
ew_round_prob <- function(p, S) {
  stopifnot(is.numeric(p))
  stopifnot(posint(S))
  signif <- floor(-log10(1 / (2 * S)))
  r <- format(round(p, signif), nsmall = signif)
  r
}

#' Check whether object is vector of non negative integers
#'
#' @param i The object to check.
#'
#' @returns TRUE or FALSE
#' @export
#'
#' @examples
#'   nonnegint(c(0, 1))
nonnegint <- function(i) {
  if (!all(is.numeric(i))) {
    return(FALSE)
  }
  if (!all(i >= 0)) {
    return(FALSE)
  }
  if (!all((round(i) == i))) {
    return(FALSE)
  }

  TRUE
}

#' Check whether object is vector of positive integers
#'
#' @param i The object to check.
#'
#' @returns TRUE or FALSE
#' @export
#'
#' @examples
#'   posint(c(1, 2))

posint <- function(i) {
  nonnegint(i) && all(i > 0)
}

#' Checks if a function is monotone rising over a specified interval.
#'
#' @param f The function to test. It should be a function that takes a numeric
#'   vector and returns a numeric vector.
#' @param range A numeric vector of length 2, defining the start and end of the
#'   interval to check (e.g., c(0, 100)).
#' @param n_points The number of points to sample within the range. The more
#'   points, the more thorough the check.
#' @param strictly A boolean. If TRUE, checks for strictly increasing (f(x2) > f(x1)).
#'   If FALSE (default), checks for non-decreasing (f(x2) >= f(x1)).
#' @return TRUE if the function is monotone rising on the sampled points, FALSE otherwise.
#' @export
#'
#' @examples
#'   f_linear_large <- function(x) 2 * x + 5
#'   should_be_true <- is_monotone_rising(f_linear_large,
#'                                        range = c(-1e6, 1e6),
#'                                        n_points = 1000)

is_monotone_rising <- function(f,
                               range = c(0, 1),
                               n_points = 1e3,
                               strictly = FALSE) {
  # 1. Generate a sequence of ordered input values
  x <- seq(from = range[[1]],
           to = range[[2]],
           length.out = n_points)

  # 2. Compute the function's output, handling errors for individual points
  y <- double(n_points)
  for (i in seq_along(x)) {
    result_i <- tryCatch(
      f(x[[i]]),
      error = function(e) {
        warning(
          sprintf(
            "The function 'f' produced a fatal error for input %f: %s",
            x[[i]],
            conditionMessage(e)
          )
        )
        return(NA_real_)
      }
    )

    # Validate the result of each call, now explicitly checking for NaN
    if (!is.numeric(result_i) ||
        length(result_i) != 1 || is.nan(result_i)) {
      # Suppress warnings for non-numeric/scalar, as they're now
      # handled explicitly
      if (!any(is.nan(result_i))) {
        warning(
          sprintf(
            "The function 'f' returned a non-numeric or non-scalar
             value for input %s.",
            paste(x[[i]])
          )
        )
      }
      y[[i]] <- NA_real_
    } else {
      y[[i]] <- result_i
    }
  }

  # NEW: If all points resulted in NA, the function is not monotone.
  if (all(is.na(y))) {
    return(FALSE)
  }

  # 3. Calculate the differences between consecutive output values
  dy <- diff(y)

  # 4. Check if all differences meet the condition, ignoring NA values
  if (strictly) {
    all(dy > 0, na.rm = TRUE)
  } else {
    all(dy >= 0, na.rm = TRUE)
  }
}

#' Find input and output pairs of a function that are not monotone rising.
#'
#' Gather pairs of input and output in a would be monotone rising
#' function where the function is not monotone rising.
#' This function is useful for debugging purposes.
#' Show the pairs of monotone rising inputs, with their non rising outputs.
#'
#' @param f The function to test. It should be a function that takes a numeric
#'   vector and returns a numeric vector.
#' @param in_vals Vector of numeric input values for f.
#' @param strictly A boolean. If TRUE, checks for strictly
#'   increasing (f(x2) > f(x1)).
#'   If FALSE (default), checks for non-decreasing (f(x2) >= f(x1)).
#' @returns A data frame with columns `input1`, `output1`, `input2`, `output2`
#'   containing the offending pairs of input and their outputs.
#' @export
#' @examples
#' # Example function that is not monotone rising
#' example_function <- function(x) {
#'   ifelse(x < 0.5, x, 1 - x)
#' }
#'
#' # Find non-monotone pairs in the example function.
#' x <- find_non_monotone_pairs(example_function, seq(
#'   from = 0,
#'   to = 1,
#'   length.out = 100), strictly = FALSE)
#' @importFrom tibble as_tibble
#' @importFrom dplyr %>%
find_non_monotone_pairs <- function(f, in_vals, strictly = FALSE) {
  # Check that in_vals is strictly monotone rising!
  stopifnot(!is.unsorted(in_vals, strictly = TRUE))

  # Compute the function's output.
  y <- double(length(in_vals))
  for (i in seq_along(in_vals)) {
    y[[i]] <- f(in_vals[[i]])
  }

  # Calculate the differences between consecutive output values
  dy <- diff(y)

  # Find pairs where the monotonicity condition is violated
  if (strictly) {
    indices <- which(dy <= 0)
  } else {
    indices <- which(dy < 0)
  }

  if (length(indices) == 0) {
    r <-
      data.frame(
        input1 = numeric(0),
        output1 = numeric(0),
        input2 = numeric(0),
        output2 = numeric(0),
        violation_size = numeric(0)
      )
    r <- tibble::as_tibble(r)
    return(r)
  }

  # Create a data frame with the offending pairs
  r <- data.frame(
    input1 = in_vals[indices],
    output1 = y[indices],
    input2 = in_vals[indices + 1],
    output2 = y[indices + 1]
  )

  # Add to r the difference in output as column.
  r$violation_size <- r$output1 - r$output2

  # Sort on violation_size, descending.
  r <- r[order(-r$violation_size), ]

  r <- tibble::as_tibble(r)
}

# This supresses the message from devtools::check().
#    Namespaces in Imports field not imported from:
#      ‘testthat’
#    All declared Imports should be used.
#
# See page 160 of "R Packages", second edition
# by Hadley Wickham and Jennifer Bryan.
ignore_unused_imports <- function() {
  testthat::test_that
}
