#' highest part of the graph with a given cumulative chance density
#'
#' @param high_density_area When 1 return \code{g} as is.
#'     When smaller than 1, the highest part of the ewgraph \code{g}
#'     will be shown
#'     which has a cumulative chance density equal to \code{high_density_interval}
#'     The idea is that this cuts away the uninteresting low value left
#'     and/or right part of the graph,
#'     and leaves only the rest.
#' @param g An ewgraph.
#' @returns A vector of the form
#'          \code{c(left = i_left, right = i_right)}.
#'          Here \code{i_left} and \code{i_right} are respectively
#'          the leftmost and rightmost ew segment of the HDI.
#' @export
#'
#' @examples
#'   S <- 1000
#'   g <- ew_from_vec(dbinom(30, 300, partition_0_1(S)))
#'   lr  <- ew_high_density_interval(g)
#'   print(lr)
ew_high_density_interval <- function(g, high_density_area = .999) {
  # Argument check.
  stopifnot(length(high_density_area) == 1)
  stopifnot(0 < high_density_area)
  stopifnot(high_density_area <= 1)
  stopifnot(ew_validate(g))

  # We implement ew_high_density_interval() by keeping on removing
  # segments at the begin or end of g till we have removed at least
  # 1 - high_density_area in chance density.
  # In each step we remove the segment with the smallest chance density surface.
  {
    surface <- ew_get_surface(g)
    first_seg <- 1
    last_seg <- ew_S(g)
    removed_area <- 0
    to_remove_area <- 1 - high_density_area

    # The following variable is toggled when the
    # surface of first_seg and last_seg are equal.
    # This ensures some symmetry.
    take_first <- TRUE

    while (first_seg <= last_seg) {
      if (first_seg == last_seg) {
        break
      } else if (to_remove_area <= removed_area) {
        break
      } else if (surface[[first_seg]] < surface[[last_seg]]) {
        removed_area <- removed_area + surface[[first_seg]]
        first_seg <- first_seg + 1
      } else if (surface[[first_seg]] > surface[[last_seg]]) {
        removed_area <- removed_area + surface[[last_seg]]
        last_seg <- last_seg - 1
      } else if (surface[[first_seg]] == surface[[last_seg]] &
                 take_first) {
        removed_area <- removed_area + surface[[first_seg]]
        first_seg <- first_seg + 1
        take_first <- FALSE
      } else if (surface[[first_seg]] == surface[[last_seg]] &
                 !take_first) {
        removed_area <- removed_area + surface[[last_seg]]
        last_seg <- last_seg - 1
        take_first <- TRUE
      } else {
        # We should never come here.
        stopifnot(FALSE)
      }
    }
    r <- c(left = first_seg, right = last_seg)
    return(r)
  }

  # The above is O(n) in time.
  # Where n is S, the number of segments of the ewgraph.
  # We can do better, but that gives a bit more complicated algorithm.
  #
  # I have the following idea for this:
  # 1. As a first step find the location of the  top of the graph with ew_maxh().
  #    This gives us the value of p for the top of the graph, p_top.
  #    It also gives us the value of the segment number of the top, i_top.
  # 2. We now divide high_density_area into three:
  #    the part left of the top, the surface of the top itself,
  #    and the part right of the top,
  #    hdi_left, hdi_top  = surface[[i_top]], hdi_right.
  #    We divide the left and right parts proportional to respectively
  #    i_top - 1, and (S - 1) - (i_top - 1).
  # 3. We do binary search to find the start of hdi_left,
  #    and also another binary search to find the end of hdi_right.
  #    This gives us the first and last segment of the HDI.
}
