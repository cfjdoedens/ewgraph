#' Add left and right interpolated and extrapolated heights to segments of equal width graph.
#'
#' For a description of equal width probability graphs see above.
#'
#' @param g should be an equal width graph, represented as a tibble with
#'   vectors p and h.
#'
#' @returns The augmented graph as a tibble.
#'
#' Examples
#' g <- ew_add_h_leftright(g)
#' @importFrom dplyr %>%
ew_add_h_leftright <- function(g) {
  # Get number of segments, i.e. rows, from g.
  S <- ew_S(g)

  # Get h from g.
  h <- ew_get_h(g)

  # Create vectors to represent left and right
  # interpolated and extrapolated heights of the segments
  # of the graph.
  # The right of segment i is the left of segment i+1.
  h_left <- double(S)
  h_right <- double(S)

  # Compute all lefts and rights that can be interpolated.
  if (S == 1) {
    # No interpolation possible
  } else { # We can interpolate.
    # S >= 2.
    for (i in 1:(S - 1)) {
      h_right[[i]] <- (h[[i]] + h[[i + 1]]) / 2
      h_left[[i + 1]] <- h_right[[i]]
    }
  }

  # Compute the lefts and rights that need to be extrapolated.
  # There are only two (!). The first left, and the last right.
  if (S == 1) {
    h_left[[1]] <- h[[1]]
    h_right[[1]] <- h[[1]]
  } else {
    # S >= 2.
    h_left[[1]] <- (3 * h[[1]] - h[[2]]) / 2
    h_right[[S]] <- (3 * h[[S]] - h[[S - 1]]) / 2

    # Make sure that the extrapolated values are non-negative.
    if (h_left[[1]] < 0) {
      h_left[[1]] <- 0
    }
    if (h_right[[S]] < 0) {
      h_right[[S]] <- 0
    }
  }

  # Add columns h_left and h_right to g.
  g <- g %>% tibble::add_column(h_left, h_right)

  g
}
