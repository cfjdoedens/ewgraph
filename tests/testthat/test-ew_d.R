test_that("Case S == 1.", {
  v <- c(1)

  # Test.
  g <- ew_from_vec(v)
  N <- 131
  for (i in 1:N) {
    expect_equal(ew_d(g, i / N), 1)
  }
})

test_that("Case S == 2. Equal chances.", {
  v <- c(1, 1)

  # Test.
  g <- ew_from_vec(v)
  N <- 131
  for (i in 1:N) {
    expect_equal(ew_d(g, i / N), 1)
  }
})

test_that("Case S == 2. Unequal chances.
          Chance graph is the straight line y = 2*x, i.e. h = 2*p.",
          {
            v <- c(0.25, 0.75) # Or equivalently: c(1, 3).

            # Test.
            g <- ew_from_vec(v)
            N <- 131
            for (i in 0:N) {
              p <- i / N
              expect_equal(ew_d(g, p), 2 * p)
            }
          })

test_that("Case S == 2. Unequal chances.
          Chance graph is the straight line y = 2 - 2*x, i.e. h = 2 - 2*p.",
          {
            v <- c(3, 1)

            # Test.
            g <- ew_from_vec(v)
            N <- 5
            for (i in 0:N) {
              test_that("find_non_monotone_pairs() basically works", {
                # Example function that is not monotone rising
                example_function <- function(x) {
                  -x
                }

                # Find non-monotone pairs in the example function.
                x <- find_non_monotone_pairs(example_function,
                                             seq(
                                               from = 0,
                                               to = 1,
                                               length.out = 3
                                             ),
                                             strictly = FALSE)
                r <- tibble::tibble(
                  input1 = c(0, 0.5),
                  output1 = c(0, -0.5),
                  input2 = c(0.5, 1),
                  output2 = c(-0.5, -1),
                  violation_size = c(0.5, 0.5)
                )
                {
                  expect_equal(x, r)
                }
              })
              test_that("find_non_monotone_pairs() basically works", {
                # Example function that is not monotone rising
                example_function <- function(x) {
                  -x
                }

                # Find non-monotone pairs in the example function.
                x <- find_non_monotone_pairs(example_function,
                                             seq(
                                               from = 0,
                                               to = 1,
                                               length.out = 3
                                             ),
                                             strictly = FALSE)
                r <- tibble::tibble(
                  input1 = c(0, 0.5),
                  output1 = c(0, -0.5),
                  input2 = c(0.5, 1),
                  output2 = c(-0.5, -1),
                  violation_size = c(0.5, 0.5)
                )
                {
                  expect_equal(x, r)
                }
              })

            }
          })

test_that("Case S == 2. Chance graph is steap line.", {
  v <- c(0.25, 75) # So 75, not .75 !

  # Test.
  g <- ew_from_vec(v)
  N <- 131
  for (i in 1:N) {
    print(ew_d(g, i / N))
  }
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
