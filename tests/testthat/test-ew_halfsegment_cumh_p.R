test_that("Trapezium is a rectangle", {
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

  U <- 1
  h1 <- 1
  h2 <- 1
  pd <- 0.5*U
  sd <- pd*h1
  expect_equal(ew_halfsegment_cumh_p(h1 = h1, h2 = h2, sd = sd , U = U), pd)
  pd <- 0.25*U
  sd <- pd*h1
  expect_equal(ew_halfsegment_cumh_p(h1 = h1, h2 = h2, sd = sd , U = U), pd)
  pd <- 0.012345*U
  sd <- pd*h1
  expect_equal(ew_halfsegment_cumh_p(h1 = h1, h2 = h2, sd = sd , U = U), pd)

  # What happens with small U?
  U <- 1e-20
  h1 <- 1
  h2 <- 1
  pd <- 0.25*U
  sd <- pd*h1
  expect_equal(ew_halfsegment_cumh_p(h1 = h1, h2 = h2, sd = sd, U = U), pd)
  # Smaller values for U give errors:
  # U <- 1e-21
  # expect_equal(ew_halfsegment_cumh_p(h1 = h1, h2 = h2, sd = sd, U = U), pd)

  # sd > surface, should throw an error.
  U <- 1
  h1 <- 1
  h2 <- 1
  pd <- 0.5*U
  sd <- 2
  expect_error(ew_halfsegment_cumh_p(h1 = h1, h2 = h2, sd = sd, U = U))
})

test_that("h1 < h2 Equilateral right triangle.", {
  U <- 1
  h1 <- 0
  h2 <- 1
  pd <- 0.5*U
  sd <- (pd^2) / 2
  expect_equal(ew_halfsegment_cumh_p(h1 = h1, h2 = h2, sd = sd, U = U), pd)

  # What happens with small U?
  U <- 1e-40
  h1 <- 0
  h2 <- U
  pd <- 0.5*U
  sd <- (pd^2) / 2
  expect_equal(ew_halfsegment_cumh_p(h1 = h1, h2 = h2, sd = sd, U = U), pd)
})

test_that("h1 > h2 Equilateral right triangle.", {
  U <- 1
  h1 <- 1
  h2 <- 0
  pd <- 0.5*U
  sd <- (U^2 / 2) - (pd^2) / 2
  expect_equal(ew_halfsegment_cumh_p(h1 = h1, h2 = h2, sd = sd, U = U), pd)

  # What happens with small U?
  U <- 1e-40
  h1 <- U
  h2 <- 0
  pd <- 0.5*U
  sd <- (U^2 / 2) - (pd^2) / 2
  expect_equal(ew_halfsegment_cumh_p(h1 = h1, h2 = h2, sd = sd, U = U), pd)
})

