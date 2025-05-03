#' Partition \[0,1\] into S equally sized consecutive segments
#'
#' Each segment is represented by its midpoint.
#'
#' @param S An integer >= 1. The number of segments to partition \[0,1\] into.
#' @returns The vector of segment midpoints,
#'          c(\code{(1-0.5)/S}, \code{(2-0.5)/S}, ... \code{(S-0.5)/S}).
#' @export
#' @examples
#'   # Returns the vector c(0.1, 0.3, 0.5, 0.7, 0.9).
#'   partition_0_1(S = 5)
partition_0_1 <- function(S = 1000) {
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
