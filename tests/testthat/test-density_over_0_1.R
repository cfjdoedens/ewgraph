test_that("1 value in [0,1] for 1 bucket", {
  y <- partition_0_1(1)
  x <- density_over_0_1(values_from_0_1 = y, S = 1)

  expect_equal(x, 1)
})

test_that("13 evenly spread values in [0,1] for 11 buckets", {
  y <- partition_0_1(13)
  x <- density_over_0_1(values_from_0_1 = y, S = 11)

  expect_equal(x, c(1, 1, 2, 1, 1, 1, 1, 1, 2, 1, 1))
})

test_that("values outside [0, 1] should not be accepted: negative values", {
  y <- c(0.1, -0.22)
  x <- expect_error(density_over_0_1(values_from_0_1 = y, S = 11))
})

test_that("values outside [0, 1] should not be accepted: positive values", {
  y <- c(0.1, 2)
  x <- expect_error(density_over_0_1(values_from_0_1 = y, S = 11))
})

test_that("large input data set", {
  y <- partition_0_1(1e8)
  x <- density_over_0_1(values_from_0_1 = y, S = 1e6)
  expect_equal(x, rep.int(100, 1e6))
})

test_that("correct handling of 0 and 1", {
  y <- c(0, 0, 0, 0.1, 0.2, 0.4, 0.9, 1, 1)
  x <- density_over_0_1(values_from_0_1 = y, S = 4)
  expect_equal(x, c(5, 1, 0, 3))
})
