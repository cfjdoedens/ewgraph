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
})

test_that("tests for partition_0_1()", {
  expect_equal(partition_0_1(S = 1), c(0.5))
  expect_equal(partition_0_1(S = 5), c(0.1, 0.3, 0.5, 0.7, 0.9))
})
