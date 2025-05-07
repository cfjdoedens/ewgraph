#' Create an equal width probability graph from vector `v`
#'
#' We assume that the elements of the vector
#' represent the probabilities p, of 1/S - 1/2S, ..., S/S - 1/2S.
#' `S` is the number of elements, so the length, of the vector.
#' We call the corresponding elements of the equal width probability
#' graph _segments_ of the probability graph.
#'
#' @param v The input vector.
#' @returns An equal width chance graph representation of the given vector.
#' @export
#' @importFrom dplyr tibble
ew_from_vec <-
  function(v) {
    S <- length(v)

    # Check v.
    stopifnot(is.numeric(v)) # v should be a numeric vector.
    stopifnot(all(0 <= v)) # v should contain only non negative numbers.
    stopifnot(1 <= S)  # v should contain at least 1 element.
    stopifnot(0 < sum(v)) # At least one of v should be > 0.

    # Create initial histogram from v by dividing [0,1] into S segments.
    p <- partition_0_1(S)
    h <- v

    # Combine vectors p and h into one tibble.
    r <- tibble(p, h)

    # Add h_left and h_right to each segment of the graph.
    # So now each segment can be considered a double trapezium.
    # The left trapezium has parallel sides with length
    # h_left and h.
    # The right trapezium has parallel sides with length
    # h and h_right.
    # The two trapeziums have the line with length h
    # in common.
    # The parallel sides make a right angle to the base line,
    # which is the p-axis.
    r <- ew_add_h_leftright(r)

    # Add surface and cumulative surface to each segment
    # of the graph.
    # The sum of the surfaces of all the trapeziums
    # is normalized to 1.
    r <- ew_add_surface(r)

    # Designate r as of class ew_probability_graph.
    class(r) <- c("ew_probability_graph", class(r))

    # Check.
    stopifnot(ew_validate(r))

    r
  }
