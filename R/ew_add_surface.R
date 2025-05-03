#' Add surface and cumulative surface to segments of equal width graph.
#'
#' This also means adapting the columns h, h_left and h_right
#' in order to normalize the total chance mass to 1.
#'
#' @param g should be an equal width graph, represented as a tibble with
#' vectors p and h, h_left and h_right.
#'
#' For a description of equal width probability graphs see ew_minmaxcumh_p.R.
#'
#' @returns
#' The augmented graph as a tibble.
#'
#' Examples
#' g <- ew_add_surface(g)
#' @importFrom dplyr near
ew_add_surface <- function(g) {
  # Get number of segments, i.e. rows, from g.
  S <- ew_S(g)

  # Compute U, i.e. half of 1/S.
  U <- 1/(2*S)

  # Create vectors to represent surface of the segments of the graph.
  raw_surface <- double(S)
  surface <- double(S)

  # Compute not yet normalized surfaces.
  h_left <- ew_get_h_left(g)
  h_right <- ew_get_h_right(g)
  h <- ew_get_h(g)
  raw_surface <- (h_left + h)*U/2 + (h + h_right)*U/2

  # Normalize surface, h, h_left, and h_right.
  raw_chance_mass <- sum(raw_surface)
  stopifnot(raw_chance_mass > 0)
  surface <- raw_surface / raw_chance_mass
  h <- h / raw_chance_mass
  h_left <- h_left/raw_chance_mass
  h_right <- h_right / raw_chance_mass
  stopifnot(near(surface, (h_left + h)*U/2 + (h + h_right)*U/2))

  # Create cumulative surface column.
  cumsurface <- cumsum(surface)

  # Replace adapted columns in g.
  g$h <- h
  g$h_left <- h_left
  g$h_right <- h_right
  g$surface <- surface
  g$cumsurface <- cumsurface

  g
}
