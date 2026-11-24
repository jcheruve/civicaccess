dims <- list(associational = c("nonprofit_access", "youth_org_access"),
             public = c("library_access", "school_access"),
             connective = "broadband_share")

test_that("normalize_indicator methods return expected values", {
  x <- c(1, 2, 3)
  expect_equal(normalize_indicator(x, "percentile"), c(0, 50, 100))
  expect_equal(normalize_indicator(x, "minmax"), c(0, 0.5, 1))
  expect_equal(normalize_indicator(x, "zscore"), c(-1, 0, 1))
  expect_equal(normalize_indicator(x, "minmax", direction = -1), c(1, 0.5, 0))
})

test_that("normalize_indicator winsorizes and handles constants and NA", {
  expect_equal(max(normalize_indicator(c(1, 2, 3, 100), "minmax", winsorize = c(0, 0.5))), 1)
  expect_equal(normalize_indicator(c(5, 5, 5), "zscore"), c(0, 0, 0))
  expect_true(is.na(normalize_indicator(c(1, NA, 3), "minmax")[2]))
  expect_error(normalize_indicator(1:3, winsorize = c(0.9, 0.1)), "increasing")
})

test_that("build_index adds dimension scores, index, and rank", {
  toy <- toy_example()
  res <- build_index(toy$tract_indicators, dims)
  expect_true(all(c("dim_associational", "dim_public", "dim_connective",
                    "civic_index_score", "civic_index_rank") %in% names(res)))
  expect_equal(res$civic_index_score,
               rowMeans(res[c("dim_associational", "dim_public", "dim_connective")]))
  # Tract with the highest values on every indicator ranks first.
  expect_equal(res$civic_index_rank[which.max(res$nonprofit_access)], 1L)
})

test_that("geometric aggregation never exceeds arithmetic aggregation", {
  toy <- toy_example()
  ari <- build_index(toy$tract_indicators, dims, method = "minmax")$civic_index_score
  geo <- build_index(toy$tract_indicators, dims, method = "minmax",
                     aggregation = "geometric")$civic_index_score
  expect_true(all(geo <= ari + 1e-12))
  expect_error(build_index(toy$tract_indicators, dims, method = "zscore",
                           aggregation = "geometric"), "non-negative")
})

test_that("build_index respects dimension weights and validates inputs", {
  toy <- toy_example()
  res <- build_index(toy$tract_indicators, dims,
                     dim_weights = c(associational = 1, public = 0, connective = 0))
  expect_equal(res$civic_index_score, res$dim_associational)
  expect_error(build_index(toy$tract_indicators, list(a = "not_a_column")), "not found")
  expect_error(build_index(toy$tract_indicators, dims, dim_weights = c(a = 1)), "named")
})

test_that("simulate_index_uncertainty returns ordered rank intervals", {
  toy <- toy_example()
  res <- simulate_index_uncertainty(toy$tract_indicators, dims, id = GEOID,
                                    n_draws = 40, seed = 42)
  expect_equal(nrow(res), nrow(toy$tract_indicators))
  expect_true(all(res$rank_low <= res$median_rank & res$median_rank <= res$rank_high))
  res2 <- simulate_index_uncertainty(toy$tract_indicators, dims, id = GEOID,
                                     n_draws = 40, seed = 42)
  expect_equal(res, res2)
})
