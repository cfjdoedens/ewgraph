#' Probability density function for ew_graph
#'
#' Approximate the density of an ew_graph anywhere on the
#' line \[0, 1\]. In other words return the (approximated) chance density of the
#' ew_graph g for any value of p on \[0, 1\].
#'
#' @param g An ew_graph.
#' @param p A real value from \[0, 1\].
#' @returns The (approximated) chance density for p on g.
#' @export
ew_d <- function(g, p) {
  # Check input parameters.
  ew_validate(g)
  stopifnot(is.numeric(p))
  stopifnot(0 <= p)
  stopifnot(p <= 1)

  # Find index, i, of segment in which p lies.
  # U == 1/(2*S), i.e. U is halve the length of a segment.
  # The segment i in which p lies has the form:
  #     |              |              |
  #  (i-1)/S      (i-1)/S + U        i/S
  #
  # So
  #  (i-1)/S    <=     p       <=    i/S
  #    i-1      <=    S*p      <=     i     [multiplication with S]
  # ceil(i-1)   <= ceil(S*p)   <=  ceil(i)  [application of ceil]
  #    i-1      <= ceil(S*p)   <=     i     [ceil(n) == n, for n integer]
  S <- ew_S(g)
  i <- max(1, ceiling(S * p))

  # Get left, middle, and right chance density of i.
  h_left  <- ew_get_h_left(g)
  h_middle <- ew_get_h(g)
  h_right <- ew_get_h_right(g)
  h_left_i <- h_left[[i]]
  h_middle_i <- h_middle[[i]]
  h_right_i <- h_right[[i]]

  # Interpolate for chance density of p.
  # p lies in segment i.
  # Either in the left part or in the right part.
  # Fringe cases are:
  # - p is left of segment
  # - p is middle of segment
  # - p is right of segment
  U <- 1 / (2 * S)
  p_left_i <- (i - 1) / S # Left of segment.
  p_middle_i <- (i - 1 / 2) / S # Middle of segment.
  p_right_i <- i / S # Right of segment
  if (near(p, p_left_i)) {
    h_p <- h_left_i
  } else if (near(p, p_middle_i)) {
    h_p <- h_middle_i
  } else if (near(p, p_right_i)) {
    h_p <- h_right_i
  } else if (p_left_i < p && p < p_middle_i) {
    interpolation_factor <- (p - p_left_i) / U
    if (h_left_i < h_middle_i) {
      h_p <- h_left_i + (h_middle_i - h_left_i) * interpolation_factor
    } else {
      h_p <- h_left_i + (h_left_i - h_middle_i) * interpolation_factor
    }
  } else if (p_middle_i < p && p < p_right_i) {
    interpolation_factor <- (p - p_middle_i) / U
    if (h_right_i < h_middle_i) {
    h_p <- h_middle_i + (h_middle_i - h_right_i) * interpolation_factor
    } else {
      h_p <- h_middle_i + (h_right_i - h_middle_i) * interpolation_factor
    }
  } else {
    stop("ew_d: p not in expected segment")
  }

  stopifnot(0 <= h_p)
  return(h_p)
}
