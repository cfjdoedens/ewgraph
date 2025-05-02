# ew_maxh(g)
# g is an equal width probability graph.
# So it describes a chance curve.
# Return the (possibly interpolated) value of h and p for which h, h_left, or h_right
# is highest of all h, h_left and h_right.
# Return as a named list of the form c(h = h_highest, p = p_highest).
#
# For a description of equal width probability graphs see ew_minmaxcumh_p.R.
#
ew_maxh <- function(g) {
  h <- ew_get_h(g)
  h_left <- ew_get_h_left(g)
  h_right <- ew_get_h_right(g)

  p <- ew_get_p(g)
  S <- ew_S(g)
  U <- 1 / (2 * S)

  if (min(h) == max(h) &&
      min(h_left) == max(h_left) && min(h_right) == max(h_right)) {
    # Chance curve ew is a straight horizontal line.

    h_highest <- 1
    p_highest <- 0.5 # Interpolated, i.e. not necessarily a value of h[[i]] for some i.

    return(c(h = h_highest, p = p_highest))
  }

  i_max_h_left <- which.max(h_left)
  i_max_h <- which.max(h)
  i_max_h_right <- which.max(h_right)

  max_hs <- c(l = h_left[[i_max_h_left]], m = h[[i_max_h]], r = h_right[[i_max_h_right]])
  max_hs_sorted <- sort(max_hs, decreasing = TRUE)
  highest <- names(max_hs_sorted)[1]

  if (highest == "l") {
    # Interpolated, i.e. not necessarily a value of h[[i]] for some i.
    h_highest <- max_hs[["l"]]
    p_highest <- p[[i_max_h_left]] - U # Shift to left 1/2S value of p.
  } else if (highest == "m") {
    # Not interpolated.
    h_highest <- max_hs[["m"]]
    p_highest <- p[[i_max_h]]
  } else  if (highest == "r") {
    # Interpolated, i.e. not necessarily a value of h[[i]] for some i.
    h_highest <- max_hs[["r"]]
    p_highest <- p[[i_max_h_right]] + U # Shift to right 1/2S value of p.
  } else {
    stop("ew_maxh(): fall through; should not come here")
  }

  return(c(h = h_highest, p = p_highest))
}
