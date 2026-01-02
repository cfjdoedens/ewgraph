test_that("tests for unity()", {
  expect_equal(1, unity(-0.034))
  expect_equal(1, unity(rnorm(1, mean = 10^10, sd = 10^20)))
  expect_equal(1, unity(NaN))
  expect_equal(1, unity(Inf))
  expect_equal(c(1, 1),  unity(c(3, 7)))
  expect_error(unity("a"))
})

test_that("tests for partition()", {
  expect_equal(partition(S = 1), c(0.5))
  expect_equal(partition(S = 5), c(0.1, 0.3, 0.5, 0.7, 0.9))
  expect_equal(partition(begin = 0, end = 0.2, S = 5), c(0.02, 0.06, 0.10, 0.14, 0.18))
  expect_equal(partition(begin = 0.1, end = 0.9, S = 5), c(0.18, 0.34, 0.50, 0.66, 0.82))
})

test_that("tests for partition_0_1()", {
  expect_equal(partition_0_1(S = 1), c(0.5))
  expect_equal(partition_0_1(S = 5), c(0.1, 0.3, 0.5, 0.7, 0.9))
})

test_that("find_non_monotone_pairs() basically works", {
  # Example function that is not monotone rising
  example_function <- function(x) {
    -x
  }

  # Find non-monotone pairs in the example function.
  x <- find_non_monotone_pairs(
    example_function,
    seq(from = 0,
    to = 1,
    length.out = 3),
    strictly = FALSE
  )
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

test_that("is_monotone_rising works for basic cases", {
  # Define functions for testing
  f_linear <- function(x) 2 * x + 5
  f_quadratic <- function(x) x^2
  f_sin <- sin

  # A linear function with a positive slope is always monotone
  expect_true(is_monotone_rising(f_linear, range = c(-10, 10)))

  # sin(x) is not monotone over [0, 2*pi]
  expect_false(is_monotone_rising(f_sin, range = c(0, 2 * pi)))

  # But sin(x) IS monotone on [0, pi/2]
  expect_true(is_monotone_rising(f_sin, range = c(0, pi / 2)))

  # x^2 is not monotone on [-1, 1], but is monotone on [0, 1]
  expect_false(is_monotone_rising(f_quadratic, range = c(-1, 1)))
  expect_true(is_monotone_rising(f_quadratic, range = c(0, 1)))
})

test_that("is_monotone_rising handles the 'strictly' argument correctly", {
  f_floor <- floor
  f_linear <- function(x) 2 * x + 5

  # A step function is non-decreasing but not strictly increasing
  expect_true(is_monotone_rising(f_floor, range = c(0, 10), strictly = FALSE))
  expect_false(is_monotone_rising(f_floor, range = c(0, 10), strictly = TRUE))

  # A linear function is both non-decreasing and strictly increasing
  expect_true(is_monotone_rising(f_linear, range = c(0, 10), strictly = FALSE))
  expect_true(is_monotone_rising(f_linear, range = c(0, 10), strictly = TRUE))
})

test_that("is_monotone_rising handles invalid function outputs gracefully", {
  # Test a function that returns the wrong data type
  f_bad_type <- function(x) "this is not numeric"
  expect_false(expect_warning(is_monotone_rising(f_bad_type)))

  # Test a function that returns a vector of the wrong length
  f_bad_length <- function(x) x[1:2]
  expect_false(expect_warning(is_monotone_rising(f_bad_length, n_points = 10)))

  # CORRECTED TEST: Test a function that produces NaNs and a warning
  f_error <- function(x) log(-1)
  expect_false(expect_warning(is_monotone_rising(f_error), "NaNs produced"))
})

test_that("is_monotone_rising handles large ranges and many points", {
  # A simple linear function over a large range
  f_linear_large <- function(x) 2 * x + 5
  expect_true(is_monotone_rising(f_linear_large, range = c(-1e6, 1e6), n_points = 1000))

  # A more complex function that is still monotone
  f_complex <- function(x) sin(x / 1e6) + x / 1e6
  expect_true(is_monotone_rising(f_complex, range = c(-1e6, 1e6), n_points = 1000))
})

test_that("find_non_monotone_pairs works for basic cases", {
  # Define functions for testing
  f_linear <- function(x) 2 * x + 5
  f_quadratic <- function(x) x^2
  f_sin <- sin

  # A linear function with a positive slope is always monotone
  expect_true(nrow(find_non_monotone_pairs(f_linear, seq(from = -10, to = 10, length.out = 1000), strictly = FALSE)) == 0)

  # sin(x) is not monotone over [0, 2*pi]
  expect_true(nrow(find_non_monotone_pairs(f_sin, seq(from = 0, to = 2 * pi, length.out = 1000), strictly = FALSE)) > 0)

  # But sin(x) IS monotone on [0, pi/2]
  expect_true(nrow(find_non_monotone_pairs(f_sin, seq(from = 0, to = pi/2, length.out = 1000), strictly = FALSE)) == 0)

  # x^2 is not monotone on [-1, 1], but is monotone on [0, 1]
  expect_true(nrow(find_non_monotone_pairs(f_quadratic, seq(from = -1, to = 1, length.out = 1000), strictly = FALSE)) > 0)
  expect_true(nrow(find_non_monotone_pairs(f_quadratic, seq(from = 0, to = 1, length.out = 1000), strictly = FALSE)) == 0)
})

