test_that("recode_binary maps codes and leaves unexpected codes missing", {
  expect_equal(recode_binary(c(1, 2, 2, 9, NA), yes = 1, no = 2), c(1L, 0L, 0L, NA, NA))
  expect_equal(recode_binary(c("Yes", "No", "DK"), yes = "Yes", no = "No"), c(1L, 0L, NA))
  expect_error(recode_binary(1:2, yes = 1, no = 1), "share")
})

test_that("survey helpers return tidy weighted estimates", {
  skip_if_not_installed("survey")
  data(api, package = "survey", envir = environment())
  apistrat$high_api <- as.integer(apistrat$api00 > 700)
  des <- survey::svydesign(ids = ~1, strata = ~stype, weights = ~pw, data = apistrat)

  props <- svy_prop_by_group(des, "high_api", "stype")
  expect_equal(nrow(props), 3)
  expect_true(all(props$ci_low <= props$estimate & props$estimate <= props$ci_high))

  ors <- svy_logit_or(des, "high_api", "meals", controls = c("ell", "stype"))
  expect_equal(ors$term, "meals")
  expect_lt(ors$odds_ratio, 1)
  expect_true(ors$conf_low <= ors$odds_ratio && ors$odds_ratio <= ors$conf_high)

  ors_f <- svy_logit_or(des, "high_api", "stype")
  expect_equal(nrow(ors_f), 2)
})
