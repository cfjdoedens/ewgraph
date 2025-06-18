test_that("Case S == 1. high_density_interval anything above 0", {
  # Create equal width probability graph g to be used for testing.
  v <- c(1)
  g <- ew_from_vec(v)

  # Test.
  lr  <- ew_high_density_interval(g, high_density_area = 0.0001)

  # Check result.
  expect_equal(lr, c(left = 1, right = 1))

  # Test.
  lr  <- ew_high_density_interval(g, high_density_area = 0.001)

  # Check result.
  expect_equal(lr, c(left = 1, right = 1))

  # Test.
  lr  <- ew_high_density_interval(g, high_density_area = 0.01)

  # Check result.
  expect_equal(lr, c(left = 1, right = 1))

  # Test.
  lr  <- ew_high_density_interval(g, high_density_area = 0.1)

  # Check result.
  expect_equal(lr, c(left = 1, right = 1))

  # Test.
  lr  <- ew_high_density_interval(g, high_density_area = 1)

  # Check result.
  expect_equal(lr, c(left = 1, right = 1))
})

test_that("Case S == 1. high_density_interval == 0", {
  # Create equal width probability graph g to be used for testing.
  v <- c(1)
  g <- ew_from_vec(v)

  # Test.
  expect_error(ew_high_density_interval(g, high_density_area = 0))
 })

test_that("Flat probability graph. S == 100000.", {
  # Create equal width probability graph g to be used for testing.
  g <- ew_from_vec(rep(1, 100000))

  # Test.
  lr  <- ew_high_density_interval(g, high_density_area = 0.5)

  # Check result.
  expect_equal(lr, c(left = 25001, right = 75000))
})

test_that("Probability graph trapezium. S == 30", {
  # Create equal width probability graph g to be used for testing.
  g <- ew_from_vec(c(0:9, rep(9, 10), 9:0))

  # Test.
  lr  <- ew_high_density_interval(g, high_density_area = 0.5)

  # Check result.
  expect_equal(lr, c(left = 11, right = 20))
})

test_that("Symmetric probability graph linear up and linear down. S == 100000.", {
  # Create equal width probability graph g to be used for testing.
  g <- ew_from_vec(c(1:50000, 50000:1))

  # Test.
  lr  <- ew_high_density_interval(g, high_density_area = 0.5)

  # Check result.
  expect_equal(lr, c(left = 35356, right = 64644))
})

