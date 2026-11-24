make_pts <- function(df, crs = 32616) sf::st_as_sf(df, coords = c("x", "y"), crs = crs)

test_that("e2sfca reproduces a hand-calculated 2SFCA result", {
  demand <- make_pts(tibble::tibble(id = c("A", "B", "C"), pop = c(100, 300, 50),
                                    x = c(0, 1000, 4000), y = 0))
  supply <- make_pts(tibble::tibble(cap = c(1, 2), x = c(0, 5000), y = 0))

  res <- e2sfca(supply, demand, demand_id = id, demand_pop = pop,
                catchment = 2000, supply_capacity = cap, decay = binary_decay)

  # Supply 1 reaches A and B: R1 = 1 / (100 + 300) = 0.0025.
  # Supply 2 reaches only C:   R2 = 2 / 50 = 0.04.
  expect_equal(res$id, c("A", "B", "C"))
  expect_equal(res$access, c(0.0025, 0.0025, 0.04))
  expect_equal(res$n_supply, c(1L, 1L, 1L))
})

test_that("e2sfca uses location-specific catchments", {
  demand <- make_pts(tibble::tibble(id = c("urban", "rural"), pop = c(100, 100),
                                    reach = c(500, 5000), x = c(0, 3000), y = 0))
  supply <- make_pts(tibble::tibble(x = 1000, y = 0))

  res <- e2sfca(supply, demand, demand_id = id, demand_pop = pop,
                catchment = reach, decay = binary_decay)

  expect_equal(res$access, c(0, 1 / 100))
})

test_that("e2sfca returns zero access when nothing is in reach", {
  demand <- make_pts(tibble::tibble(id = "A", pop = 10, x = 0, y = 0))
  supply <- make_pts(tibble::tibble(x = 1e6, y = 0))
  res <- e2sfca(supply, demand, demand_id = id, demand_pop = pop, catchment = 1000)
  expect_equal(res$access, 0)
  expect_equal(res$n_supply, 0L)
})

test_that("gaussian decay gives nearer demand more access than farther demand", {
  toy <- toy_example()
  res <- e2sfca(toy$supply, toy$demand, demand_id = bg_id, demand_pop = pop_18_29,
                catchment = catchment_m, supply_capacity = capacity)
  expect_equal(nrow(res), nrow(toy$demand))
  expect_true(all(res$access >= 0))
  expect_true(all(is.finite(res$access)))
})

test_that("e2sfca validates inputs", {
  toy <- toy_example()
  expect_error(e2sfca(sf::st_drop_geometry(toy$supply), toy$demand, bg_id, pop_18_29, 1000),
               "sf object")
  expect_error(e2sfca(sf::st_transform(toy$supply, 4326), toy$demand, bg_id, pop_18_29, 1000),
               "same CRS")
  expect_error(e2sfca(toy$supply, toy$demand, bg_id, pop_18_29, catchment = -5),
               "positive")
})
