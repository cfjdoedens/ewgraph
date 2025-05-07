#' Return TRUE iff the highest value of h is reached
#' neither for the h_left of the first segment
#' nor for the h_right of the last segment
#'
#' @param g An ewgraph.
#'
#' @returns TRUE or FALSE.
#' @export
#'
#' @examples
#'   S <- 1000
#'   g <- ew_from_vec(dbinom(30, 300, partition_0_1(S)))
#'   middle_peaked <- ew_middle_peaked(g)
#'   print(middle_peaked)
ew_middle_peaked <- function(g) {
  # Get h_left.
  h_left <- ew_get_h_left(g)

  # Get index of segment with highest value for h_left.
  i_max_h_left <- which.max(h_left)

  # Get h_right.
  h_right <- ew_get_h_right(g)

  # Get index of segment with highest value for h_right.
  i_max_h_right <- which.max(h_right)

  # Check that neither h_left[[1]] nor h_right[[S]] is the highest value.
  S <- ew_S(g)
  i_max_h_left != 1 && i_max_h_right != S
}
