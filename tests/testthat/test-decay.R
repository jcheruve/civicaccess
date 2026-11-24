test_that("gaussian_decay is 1 at zero, 0 at and beyond the threshold, and decreasing", {
  d <- c(0, 250, 500, 750, 1000, 1500)
  w <- gaussian_decay(d, d0 = 1000)
  expect_equal(w[1], 1)
  expect_equal(w[5], 0)
  expect_equal(w[6], 0)
  expect_true(all(diff(w[1:5]) < 0))
})

test_that("gaussian_decay accepts a threshold per distance", {
  w <- gaussian_decay(c(500, 500), d0 = c(1000, 400))
  expect_gt(w[1], 0)
  expect_equal(w[2], 0)
})

test_that("binary_decay is a step function", {
  expect_equal(binary_decay(c(0, 1000, 1001), d0 = 1000), c(1, 1, 0))
})

test_that("decay functions validate inputs", {
  expect_error(gaussian_decay("a", 1), "numeric")
  expect_error(gaussian_decay(1, 0), "greater than 0")
  expect_error(binary_decay(1:3, c(1, 2)), "length")
})
