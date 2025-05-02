test_that("Do not allow non numeric vector", {
  v <- c('a', 'b')

  # Test.
  expect_error(ew_from_vec(v))
})

test_that("Do not allow negative numbers in vector", {
  v <- c(0, -1)

  # Test.
  expect_error(ew_from_vec(v))
})

test_that("Do not allow empty vector", {
  v <- c()

  # Test.
  expect_error(ew_from_vec(v))
})

test_that("Do not allow all 0 vector", {
  v <- c(0, 0)

  # Test.
  expect_error(ew_from_vec(v))
})

test_that("Case S == 1.", {
  v <- c(1)

  # Test.
  g <- ew_from_vec(v)

  # Check result.
  expect_equal(as.numeric(g), c(0.5, 1, 1, 1, 1, 1))
})

test_that("Case S == 2. Equal chances.", {
  v <- c(1, 1)

  # Test.
  g <- ew_from_vec(v)

  # Check result.
  expect_equal(as.numeric(g[1, ]), c(0.25, 1, 1, 1, 0.5, 0.5))
  expect_equal(as.numeric(g[2, ]), c(0.75, 1, 1, 1, 0.5, 1))
})

test_that("Case S == 2. Unequal chances.
          Chance graph is the straight line y = x, i.e. h = p.",
          {
            v <- c(0.25, 0.75)

            # Test.
            g <- ew_from_vec(v)

            # Check result.
            expect_equal(as.numeric(g[1, ]), c(0.25, 0.5, 0, 1, 0.25, 0.25))
            expect_equal(as.numeric(g[2, ]), c(0.75, 1.5, 1, 2, 0.75, 1))
          })

test_that("Case S == 2. Provoked bug: h_left < 0. Should be fine now.", {
  v <- c(0.25, 75)

  # Test.
  g <- ew_from_vec(v)

  # Check result.
  expect_equal(as.numeric(g[1, ]),
               c(0.2500000000000000, 0.0059149722735675, 0.0000000000000000, 0.8902033271719039, 0.1127541589648798, 0.1127541589648798))
  expect_equal(as.numeric(g[2, ]),
               c(0.75000000000000, 1.77449168207024, 0.89020332717190, 2.65878003696858, 0.88724584103512, 1.00000000000000))
})

test_that("Case S == 10,000. Just to see what happens when
          processing a somewhat big ew graph",
          {
            v <- 1:1e5

            # Test.
            g <- ew_from_vec(v)

            # No checking of result.
            expect_equal(TRUE, TRUE)
          })
