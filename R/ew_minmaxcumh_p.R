# ew_maxcumh_p(g, s)
# g is an equal width probability graph, an ew_graph.
# So it describes a chance curve.
# For a description of ew_graphs see ew_add_h_leftright.R.
#
# Return the lowest (possibly interpolated) value of p for which the cumulative
# chance of h, starting from the first element of g and going to
# the last element of g is >= s
ew_maxcumh_p <- function(g, s) {
  p <- ew_get_p(g)
  h_left <- ew_get_h_left(g)
  h <- ew_get_h(g)
  h_right <- ew_get_h_right(g)
  cumsurface <- ew_get_cumsurface(g)
  surface <- ew_get_surface(g)
  S <- ew_S(g) # Number of segments of ew_graph g.
  U <- 1 / (2 * S) # Length of base line of half segment of ew_graph.

  # We look for the lowest index of a segment of g for which s <= cumsurface.
  # We do that using binary search.
  # We use the loop invariant that the segment we are looking for is in the range:
  # [begin_range, begin_range + width_range - 1].
  {
    # The first index of g from which we do binary search.
    begin_range <- 1

    # The number of segments we are still searching in.
    # We search in the range [begin_range, begin_range + width_range - 1]
    width_range <- S # So width_range >= 1.

    while (TRUE) {
      if (width_range == 1) {
        # Width of search range is 1, we are done searching
        # for the right segment.
        # The index of this segment is begin_range.
        # But we will use i, for short.
        i <- begin_range

        # Below we need to reference the cumulative surface
        # to the left of segment i, cumsurface[[i-1]].
        # However, for S == 1 there is no cumsurface[[i-1]].
        # Therefore we use cumsurface_im1 for cumsurface[[i-1]],
        # and adapt it for the special case where i == 1.
        if (i == 1) {
          cumsurface_im1 <- 0
        } else {
          cumsurface_im1 <- cumsurface[[i - 1]]
        }

        # Determine sd, the part of chance surface s inside segment i.
        {
          if (i == 1) {
            sd <- s
          } else {
            sd <- s - cumsurface_im1
          }
        }

        # Some abbreviations.
        pi <- p[[i]] # (Overrides built in value 3.14.. .)
        hli <- h_left[[i]]
        hi <- h[[i]]
        hri <- h_right[[i]]
        si <- surface[[i]]

        # The sought for value of p should be in segment i.
        # Therefore sd should not exceed si.
        stopifnot(0 <= sd)
        stopifnot(sd < si || near(sd, si)) # sd <= si, or nearly so.

        # Split si into left and right part.
        surface_left <- (hli + hi) * U / 2
        surface_right <- (hri + hi) * U / 2
        stopifnot(near(si, surface_left + surface_right))

        if (sd < surface_left) {
          # So the sought for value of p is in [pi - U, pi).

          # p_delta is the part of p that is left to reach.
          p_delta <- ew_halfsegment_cumh_p(
            h1 = hli,
            h2 = hi,
            sd = sd,
            U = U
          )
          return(pi - U + p_delta) # Remember: pi is in the middle of segment i.
        } else if (s == cumsurface_im1 + surface_left) {
          # So the sought for value of p is pi.
          return(pi)
        } else {
          # So the sought for value of p is in (pi, pi + U].

          # p_delta is the part of p that is left to reach.
          p_delta <- ew_halfsegment_cumh_p(
            h1 = hi,
            h2 = hri,
            sd = sd - surface_left,
            U = U
          )
          return(pi + p_delta) # Remember: pi is in the middle of segment i.
        }
      }  else {
        # Otherwise we split the range in two and continue.
        # We know that width_range >= 2.
        # So width_range_1 and width_range_2 will both be >= 1.
        width_range_1 <- ceiling(width_range / 2)
        width_range_2 <- width_range - width_range_1
        stopifnot(1 <= width_range_1)
        stopifnot(1 <= width_range_2)
        if (s <= cumsurface[[begin_range + width_range_1 - 1]]) {
          width_range <- width_range_1
        } else {
          width_range <- width_range_2
          begin_range <- begin_range + width_range_1
        }
      }
    }
  }
  stop("ew_maxcumh_p(): fall through; should not come here")
}

# ew_mincumh_p(g, s)
# g is an equal width probability graph.
# So it describes a chance curve.
# For a description of equal width probability graphs see the top of this file.
# Return the highest interpolated value of p for which the cumulative
# chance of h, starting from the last segment of g and going to
# the first segment of g is <= 1 - s.
ew_mincumh_p <- function(g, s) {
  ew_maxcumh_p(g, 1 - s)
}
