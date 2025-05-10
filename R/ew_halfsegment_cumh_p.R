#' In ew half segment, given cumulative value for h, find corresponding p value
#'
#' An ew half segment of an equal width graph, or ewgraph,
#' is a rectangular trapezium with
#' baseline size U and two parallel sides perpendicular
#' to the base line.
#'
#' @param h1 The height of the left side of the trapezium.
#' @param h2 The height of the right side of the trapezium.
#' @param sd Alias for surface_delta:
#'           The left part of the surface of the trapezium
#'           bordered by an imaginary line perpendicular to the
#'           base line of the trapezium.
#'           sd > 0, and smaller than the surface of the
#'           trapezium.
#' @param U  The length of the baseline of the trapezium.
#' @export
#'
#' @returns
#' The computed value of x, which is the point where the imaginary line
#' crosses the base line.
#'
#' @examples
#'   x <- ew_halfsegment_cumh_p(h1 = 1, h2 = 0, sd = 0.0125, U = 0.25)
ew_halfsegment_cumh_p <- function(h1, h2, sd, U) {
  stopifnot(length(h1) == 1)
  stopifnot(is.numeric(h1))
  stopifnot(0 <= h1)

  stopifnot(length(h2) == 1)
  stopifnot(is.numeric(h2))
  stopifnot(0 <= h2)

  stopifnot(length(sd) == 1)
  stopifnot(is.numeric(sd))
  stopifnot(0 < sd)

  stopifnot(length(U) == 1)
  stopifnot(is.numeric(U))
  stopifnot(0 < U)

  # The surface that has to be reached, surface_delta, alias sd,
  # is part of the surface of the trapezium.
  # So it should be > 0, and smaller or equal than that surface.
  stopifnot(0 < sd)
  surface_trapezium <- U * (h1 + h2) / 2
  stopifnot(sd < surface_trapezium || near(sd, surface_trapezium)) # sd <= surface_trapezium, or nearly so.

  if (h1 == h2) {
    # The trapezium is in fact a rectangle.
    #
    #    A               D
    #     ________________
    #    |               |
    #    |               |
    # h1 |               | h2
    #    |               |
    #    |               |
    #    |_______________|
    #    B               C
    #           U
    #
    # We introduce into the picture as surface(ABZY),
    # surface_delta, alias sd, the surface that has to be reached.
    # And also length(BZ), alias p_delta, alias pd, alias x,
    # the delta p that we need to reach surface_delta.
    #
    #    A         Y     D
    #     ________________
    #    |         |     |
    #    |         |     |
    # h1 |    sd   |     | h2
    #    |         |     |
    #    |         |     |
    #    |_________|_____|
    #    B    pd   Z     C
    #           U
    #
    #     (4) x = pd = length(BZ)
    #     (5) sd = surface(ABZY)
    #     (6)    = length(BZ) * length(YZ)
    #     (7)    = x * h1
    #  So
    x <- sd / h1 # (8)
  } else if (h1 < h2) {
    # So we have a trapezium, ABCD.
    # And length(AB) < length(DC).
    #
    #                   D
    #                   "
    #                  /
    #                 / |
    #                /  |
    #               /   |
    #              /    |
    #             /     |
    #            /      |
    #           /       |
    #          /        |
    #         /         |
    #        /          |
    #       /           |
    #      /            |
    #     /             |
    #    /              |
    # A |               |
    #   |               |
    #   |               |
    #   |               |
    #   |               |
    #   |_______________|
    #   B               C
    #
    #   We have:
    #       (1) h1 = length(AB)
    #       (2) h2 = length(FC)
    #
    #
    #                     D
    #                     "
    #                    /
    #                   / |
    #                  /  |
    #                 /   |
    #                /    |
    #               /     |
    #              /      |
    #             /       |
    #            /        |
    #           /         | h2
    #          /          |
    #         /           |
    #        /            |
    #       /             |
    #      /              |
    #   A |               |
    #     |               |
    #  h1 |               |
    #     |               |
    #     |               |
    #     |_______________|
    #     B               C
    #
    #
    # U is the length of the base line, BC, of the trapezium.
    #
    #                     D
    #                     "
    #                    /
    #                   / |
    #                  /  |
    #                 /   |
    #                /    |
    #               /     |
    #              /      |
    #             /       |
    #            /        |
    #           /         | h2
    #          /          |
    #         /           |
    #        /            |
    #       /             |
    #      /              |
    #   A |               |
    #     |               |
    #  h1 |               |
    #     |               |
    #     |               |
    #     |_______________|
    #     B               C
    #            U

    # Now length(AB) = h1 < length(DC) = h2.
    #
    #                     D
    #                     "
    #                    /
    #                   / |
    #                  /  |
    #                 /   |
    #              Y /    |
    #               /|    |
    #              / |    |
    #             /  |    |
    #            /   |    |
    #           /    |    | h2
    #          /     |    |
    #         /      |    |
    #        /       |    |
    #       /        |    |
    #    A /         |    |
    #     |     sd   |    |
    #     |          |    |
    # h1  |          |    |
    #     |          |    |
    #     |          |    |
    #     |__________|____|
    #     B     pd   Z    C
    #             U
    #
    # We introduce the triangles AQY and ARD.
    # They are similar.
    # This because:
    #   1. angle AQY = angle ARD = 90 degrees
    #   2. angle YAQ = angle DAR
    #
    # The surface of ABZY which is sd is now
    # split into two parts, surface(ABZQ) + surface(AQY)
    #
    # Let
    #     (9) sd1 = surface(ABZQ)
    #     (10) sd2 = surface(AQY)
    #
    #                     D
    #                     "
    #                    /
    #                   / |
    #                  /  |
    #                 /   |
    #              Y /    |
    #               /|    |
    #              / |    |
    #             /  |    | h2
    #            /   |    |
    #           /    |    |
    #          /     |    |
    #         /      |    |
    #        /  sd2  |    |
    #       /        |    |
    #    A /_________|____|R
    #     |         Q|    |
    #     |          |    |
    # h1  |    sd1   |    |
    #     |          |    |
    #     |          |    |
    #     |__________|____|
    #     B     pd   Z    C
    #             U
    #
    # Let
    #   (11) hd = length(YQ)
    #
    #                     D
    #                     "
    #                    /
    #                   / |
    #                  /  |
    #                 /   |
    #              Y /    |
    #               /|    |
    #              / |    |
    #             /  |    |  h2
    #            /   | hd |
    #           /    |    |
    #          /     |    |
    #         /      |    |
    #        /  sd2  |    |
    #       /        |    |
    #    A /_________|____|R
    #     |         Q|    |
    #     |          |    |
    # h1  |    sd1   |    |
    #     |          |    |
    #     |          |    |
    #     |__________|____|
    #     B     pd   Z    C
    #             U
    #
    # Because triangles AQY and ARD are similar we get
    #     (12) hd/pd = (h2-h1)/U
    #
    # As sd = sd1 + sd2 we get
    #     (13) sd = pd*h1+pd*hd/2
    #
    # hd and pd are unknown.
    # h1, h2, sd, and U are known.
    #
    # We solve these two equations with these two unknowns:
    # Let
    #     (14) x = pd
    #     (15) r = (h2-h1)/U
    # We then get from (12):
    #     (14) hd/x = r
    # And from (13):
    #     (15) sd = x*(h1+hd/2)
    # We solve for hd in (14).
    #     (16) hd = x*r
    # and substitute in (15).
    # We then get a quadratic equation in x:
    #     (17) sd = x*(h1 + x*r/2))
    # Which we can normalize to:
    #     (18) (r/2)*x^2 + x*h1 - sd = 0
    # We solve this equation for x.
    # We get two solutions:
    #     (19) x = (-h1 + sqrt(h1^2 + 2*r*sd))/r
    #     (20) x = (-h1 - sqrt(h1^2 + 2*r*sd))/r
    # However, (20) is invalid, as it implies that x < 0.
    # So only solution (19) is valid.
    r <- (h2 - h1) / U
    x <- (-h1 + sqrt(h1^2 + 2 * r * sd)) / r

    # Now it should hold that 0 < pd < U.
    # First we prove 0 < pd.
    #     (21) (-h1 + sqrt(h1^2 + 2*r*sd)) / r >? 0
    #     (22) -h1 + sqrt(h1^2 + 2*r*sd) >? 0
    #     (23) sqrt(h1^2 + 2*r*sd) >? h1
    #     (24) h1^2 + 2*r*sd >? h1^2
    #     (25) 2*r*sd >? 0
    #     (26) r*sd >? 0
    # Which is clearly the case as both r and sd are positive. QED.
    # Now we prove pd < U.
    #     (27) (-h1 + sqrt(h1^2 + 2*r*sd)) / r <? U
    #     (28) -h1 + sqrt(h1^2 + 2*r*sd) <? U*r
    #     (29) sqrt(h1^2 + 2*r*sd) <? h1 + U*r
    #     (30) h1^2 + 2*r*sd <? h1^2 + 2*h1*U*r+ U^2*r^2
    #     (31) 2*r*sd <? 2*h1*U*r+ U^2*r^2
    #     (32) 2*sd <? 2*h1*U + U^2*r
    #     (33) 2*sd <? 2*h1*U + U^2*(h2 - h1)/U
    #     (34) 2*sd <? 2*h1*U + U*(h2 - h1)
    #     (35) 2*sd <? h1*U + h2*U
    #     (36) sd <? (h1*U + h2*U)/2
    #     (37) sd <? surface(ABCD)
    # Which clearly is the case. QED.

    # Check whether the original equations are satisfied by the
    # solution found.
    {
      pd <- x
      hd <- x * r

      if (isFALSE(near(h2, h1, 1e-9))) {
        stopifnot(near(hd / pd, (h2 - h1) / U)) # Check (12).
        stopifnot(near(sd, pd * (h1 + hd / 2))) # Check (13).
      }
    }
    return(x)
  } else {
    # h1 > h2
    #
    # We can use the computation for h1 < h2
    # by mirroring.
    #
    #     A
    #     "
    #     |\
    #     | \
    #     |  \
    #     |   \
    #     |    \ Y
    #     |    |\
    #     |    | \
    #     |    |  \
    #     |    |   \
    #     |    |    \
    #     |    |     \
    # h1  |    |      \
    #     |    |       \
    # =   |    |        \
    #     | sd |         \
    # h2q |    |    sd'   | D
    #     |    |          |
    #     |    |          |  h2
    #     |    |          |  =
    #     |    |          |  h1q
    #     |____|__________|
    #     B    Z          C
    #       pd    pd'
    #           U
    #
    h1q <- h2
    h2q <- h1
    # sd' = surface(ABCD) - sd
    sdq <- (h1q + h2q)*U/2 - sd
    pdq <- ew_halfsegment_cumh_p(h1 = h1q, h2 = h2q, sd = sdq, U = U)
    pd <- U - pdq
    return(pd)
  }
}
