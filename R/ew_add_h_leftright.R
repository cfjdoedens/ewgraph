# _equal width probability graphs_
#
# Introduction
#
# An equal width probability graph, ew_graph, is a finite numeric
# representation of a function c from domain P to codomain H.
# c: P -> H
# P are the real numbers in [0,1].
# H are the real numbers in [0,inf].
# c is interpreted in this context as function that maps from an
# error fraction p to the chance density of that error fraction, h = c(p).
# So c is an absolutely continuous probability distribution.
# And c is a probability density function.
#
# This function, c, is represented in an ew_graph as a finite set of S pairs
# (pi, hi), i in 1:S. S a positive integer.
# pi are elements of P, so of [0, 1].
# hi are elements of H, so of [0, inf].
# hi = c(pi).
#
# Let U = 1/(2*S).
# We define the ith segment of P as [i/S - 1/S, i/S].
# So all segments have equal width, 1/S = 2*U.
# And there are S segments.
# The middle of the ith segment being pi = i/S - U.
# So the segments are: [0, 1/S], [1/S, 2/S], ..., [(S - 1)/S, 1].
#
# The surface of a segment
#
# An important aspect of a segment is its surface.
# For a probability density function, the probability that an outcome lies on
# a certain segment of P is proportional to the surface of that segment.
# We approximate the surface of a segment of the probability function
# as the surfaces of the segments of the corresponding equal width
# probability graph.
# The sum of the surfaces of all segments is by definition 1.
#
# As a first approximation we can equal the surface of a segment to
# hi * (1/S).
# See drawing below of segment i
#
#
#                   |hi
#                   |
#                   |
#                   |
#                   |
#    _______________|________________
#    pi - U         pi               pi + U
#    =              =                 =
#  i/S - 2U        i/S - U            i/S
#
#  surfacei = length of segment * height of segment
#           = (pi + U - (pi - U))) * hi
#           = 2U * hi
#           = (1/S) * hi
#
# When the probability graph is rather flat this is a nice approximation.
# However on steep parts of the graph, the approximation is less a good fit.
# To improve on this we approximate the surface of a segment by
# computing also the height of the begin and end points of
# the segment. So by computing h(pi-U) =def hlefti, and
# h(pi+U) =def hrighti.
# We can then compute the surface of segment i as the sum
# of its left and right parts.
#
#
#                   |hi
#                   |
#                   |               |hrighti
#   |hlefti         |               |
#   |               |               |
#   |_______________|_______________|
#   pi - U          pi              pi + U
#    =              =                 =
#  i/S - 2U        i/S - U            i/S
#
#  surfacei = surface of left i + surface of right i
#  surface left i  = length of left halve of segment i *
#                    (hlefti + hi)/2
#                  = U*(hlefti + hi)/2
#  surface right i = length of right halve of segment i *
#                   (hi + hrighti)/2
#                  = U*(hi + hrighti)/2
#  surfacei        = U*(hlefti + hi)/2 + U*(hi + hrighti)/2
#                  = (hlefti + hrighti + 2*hi) * U /2
#
#  We compute hlefti and hrighti by interpolation.
#
#  For hllefti we proceed:
#  Let him1 = c(pim1), where im1 stands for i - 1.
#  We then interpolate:
#  hlefti  = (hi + him1)/2
#
#                                                   |hi
#                                   |hlefti         |
#                   |him1           |               |
#                   |               |               |
#    _______________|_______________|_______________|_______________
#                   pim1            pi - U          pi
#   segment i-1                     segment i
#
#   In the same vein we get
#   hrighti = (hi + hip1)/2
#
#                   |hi
#                   |               |hrighti
#                   | hi            |               |hip1
#                   |               |               |
#    _______________|_______________|_______________|_______________
#                   pi              pim1 - U        pi
#   segment i                       segment i+1
#
#   This leaves us with the problem how to interpolate the leftmost hlefti,
#   and the rightmost hrighti, i.e. hleft1 and hrightS.
#   This because, we can not interpolate with hi from segment 0 or from
#   segment S+1, as these segments do not exist.
#   For those cases instead of interpolating we extrapolate.
#
#   For hleft1:
#
#                                                  |h2
#                                  |               |
#                  |h1             |               |
#    |hleft1       |               |               |
#    |_____________|_______________|_______________|_______________
#    0             p1              p2 - U          p2
#    segment 1                     segment 2

#   The line through the coordinates (U, h1) and (3*U, h2)
#   has the algebraic form y = ax+b.
#   So
#     h1 = a*U + b
#     h2 = a*3*U + b.
#   Then we get
#     a = (h2 - h1)/(2*U)
#     b = (3*h1 - h2)/2
#
#   So for hleft1 we get
#     hleft1 = a*0 + b
#            = b
#            = (3*h1 - h2)/2
#
#   However, note that hleft1 could become less than 0.
#   This we should forbid, as it would imply a negative probability
#   density.
#
#   So, taking this into account we get:
#     hleft1 = max(0, (3*h1 - h2)/2)
#
#   For hrightS:
#
#                  |hSm1
#                  |               |
#                  | h1            |               |hS
#                  |               |               |              |hrightS
#     _____________|_______________|_______________|______________|
#                  pSm1            pS - U          pS             1
#     segment S-1                  segment S
#
#   The line through the coordinates (1 - 3*U, hSm1) and (1 - U, hS)
#   has the algebraic form y = ax+b.
#   So
#     hSm1 = a*(1-3*U) + b
#     hS   = a*(1-U) + b.
#   Then we get
#     a = (hS - hSm1)/(2*U)
#     b = hS + (hSm1-hS)*(1-U)/(2*U)
#
#   So for hrightS we get
#     hrightS = a*1 + b
#             = a + b
#             = (hS - hSm1)/(2*U) + hS + (hSm1-hS)*(1-U)/(2*U)
#             = (3*hS - hSm1)/2
#
#     This outcome is symmetric with hleft1 = (3*h1 - h2)/2.
#
#   As with hleft1 we should forbid negative values for hrightS.
#   So, taking this into account we get:
#     hrightS = max(0, (3*hS - hSm1)/2)
#
# Representation of a segment from an ew_graph as a geometric figure
#
# See the figure below.
# A segment of an ew_graph ABDEF can be considered to consist of two rectangular
# trapeziums, ABCF and EDCF, representing respectively the left and
# right halve of the segment.
# The corners ABC and CDE are rectangular.
#
# The two trapeziums have a shared parallel line, CF, which has length hi.
# The three (one from the left trapezium, AB, one shared, CF, and one
# from the right trapezium, DE) parallel lines are perpendicular to the p-axis,
# BD.
# The lines of each trapezium BC and DC, that lie on the p-axis have
# each length U.
# Line AB has length hlefti.
# Line ED has length hrighti.
#
#                   F
#                   "
#                  / \
#                 / | \
#                /  |  \
#               /   |   \
#              /    |    \
#             /     |     \
#            /      |      \
#           /       |       \
#          /        |        \
#         /         |         \
#        /          |          \
#       /           |           \
#      /            |            \
#     /             |             \
#    /              |              \
# A |               |               \
#   |               |                \ E
#   |               |                |
#   |               |                |
#   |               |                |
#   |_______________|________________|
#   B               C                D
#
#   Figure: depiction of a segment from an ew_graph.
#
# Comparison with standard numerical approximation of an integral
#
# I cite from Wikipedia:
# In calculus, the definite integral of an arbitrary function f(x)
# can be numerically approximated as a discrete sum by partitioning the
# interval of integration into small uniform intervals and approximating
# the function's value on each interval as the average of the values
# at its endpoints.
#
# We see here the correspondence between the small uniform intervals
# used for calculating the definite integral
# and the segments of the ew_graph.
#
# In contrast to the cited method an ew_graph
# is in essence based on the middle value of a segment.
# The reason for this choice is that typically a probability graph
# in the realm we are dealing with, might have an infinite
# height at p = 0 or at p = 1. By stearing clear from these
# values, we avoid this problem. Furthermore I think
# that using the one middle value of a segment as representative
# is more elegant and more symmetric than using the values
# of the end points as representative. But this is also a matter
# of taste.
#
# Representation of ew_graph as R tibble
#
# In the programming language R we represent the ew_graph as a tibble with S rows.
# Each ith segment of the ew_graph has the following variables:
# - p[[i]] = i/S - U,
#   this means that p[[i]] is the mid point of the ith segment
# - h[[i]] = c(p[[i]])
#   so h[[i]] is the chance of p[[i]]
# - h_left[[i]] which is a linear interpolation of h[[i]] and h[[i-1]]
#   (but a linear extrapolation of h[[1]] and h[[2]] for h_left[[1]])
# - h_right[[i]] which is a linear interpolation of h[[i]] and h[[i+1]]
#   (but a linear extrapolation of h[[S]] and h[[S-1]] for h_right(S))
# - surface[[i]] which is the surface of this ith segment.
# - cumsurface[[i]] which is the cumulative of surface[[i]], going from 1 to i


#' Add left and right interpolated and extrapolated heights to segments of equal width graph.
#'
#' For a description of equal width probability graphs see above.
#'
#' @param g should be an equal width graph, represented as a tibble with
#' vectors p and h.
#'
#' @returns
#' The augmented graph as a tibble.
#'
#' Examples
#' g <- ew_add_h_leftright(g)
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
