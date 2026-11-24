#' Recode a coded survey item to a 0/1 indicator
#'
#' Survey exports often code binary items as 1/2 or carry "don't know" codes.
#' `recode_binary()` maps the listed codes to 1 and 0 and sets every other
#' value to `NA`, so unexpected codes surface as missing rather than being
#' silently folded into a category.
#'
#' @param x A vector (numeric, character, or `haven_labelled`).
#' @param yes Value(s) to code as 1.
#' @param no Value(s) to code as 0.
#'
#' @return Integer vector of 1, 0, and `NA`.
#' @family survey helpers
#' @export
#' @examples
#' recode_binary(c(1, 2, 2, 9, NA), yes = 1, no = 2)
recode_binary <- function(x, yes, no) {
  if (length(intersect(yes, no)) > 0) {
    rlang::abort("`yes` and `no` must not share values.")
  }
  x <- as.vector(unclass(x))
  dplyr::case_when(
    x %in% yes ~ 1L,
    x %in% no  ~ 0L,
    .default   = NA_integer_
  )
}

#' Survey-weighted proportions with confidence intervals by group
#'
#' Estimates the weighted proportion of a 0/1 outcome within each level of a
#' grouping variable, with logit-transformed confidence intervals from
#' [survey::svyciprop()].
#'
#' @param design A `survey.design` object from [survey::svydesign()].
#' @param outcome Name of a 0/1 outcome variable (string).
#' @param group Name of the grouping variable (string).
#' @param level Confidence level.
#'
#' @return A tibble with the group level, `estimate`, `ci_low`, and
#'   `ci_high`, all on the proportion scale.
#' @family survey helpers
#' @export
#' @examples
#' if (requireNamespace("survey", quietly = TRUE)) {
#'   data(api, package = "survey")
#'   apistrat$high_api <- as.integer(apistrat$api00 > 700)
#'   des <- survey::svydesign(ids = ~1, strata = ~stype, weights = ~pw, data = apistrat)
#'   svy_prop_by_group(des, "high_api", "stype")
#' }
svy_prop_by_group <- function(design, outcome, group, level = 0.95) {
  rlang::check_installed("survey", reason = "for survey-weighted estimates.")
  vars <- design$variables
  if (!all(c(outcome, group) %in% names(vars))) {
    rlang::abort("`outcome` and `group` must be variables in `design`.")
  }
  levels_g <- sort(unique(stats::na.omit(vars[[group]])))
  form <- stats::as.formula(paste0("~", outcome))

  purrr::map(levels_g, function(lv) {
    sub <- subset(design, design$variables[[group]] == lv)
    est <- survey::svyciprop(form, design = sub, method = "logit", level = level)
    ci <- stats::confint(est)
    tibble::tibble(!!group := lv,
                   estimate = as.numeric(stats::coef(est)),
                   ci_low = ci[1], ci_high = ci[2])
  }) |>
    purrr::list_rbind()
}

#' Survey-weighted logistic regression odds ratios for an exposure
#'
#' Fits `outcome ~ exposure + controls` with [survey::svyglm()] and a
#' quasibinomial family, and returns odds ratios with Wald confidence
#' intervals for the exposure term(s). Factor exposures return one row per
#' non-reference level.
#'
#' @inheritParams svy_prop_by_group
#' @param exposure Name of the exposure (predictor of interest), as a string.
#' @param controls Optional character vector of control variable names.
#'
#' @return A tibble with `term`, `odds_ratio`, `conf_low`, `conf_high`, and
#'   `p_value` for the exposure term(s).
#' @family survey helpers
#' @export
#' @examples
#' if (requireNamespace("survey", quietly = TRUE)) {
#'   data(api, package = "survey")
#'   apistrat$high_api <- as.integer(apistrat$api00 > 700)
#'   des <- survey::svydesign(ids = ~1, strata = ~stype, weights = ~pw, data = apistrat)
#'   svy_logit_or(des, "high_api", "meals", controls = c("ell", "stype"))
#' }
svy_logit_or <- function(design, outcome, exposure, controls = NULL, level = 0.95) {
  rlang::check_installed("survey", reason = "for survey-weighted models.")
  rhs <- paste(c(exposure, controls), collapse = " + ")
  frm <- stats::as.formula(paste(outcome, "~", rhs))
  fit <- survey::svyglm(frm, design = design, family = stats::quasibinomial(link = "logit"))

  coefs <- summary(fit)$coefficients
  ci <- stats::confint(fit, level = level)
  terms <- rownames(coefs)
  keep <- startsWith(terms, exposure)

  tibble::tibble(
    term       = terms[keep],
    odds_ratio = exp(coefs[keep, "Estimate"]),
    conf_low   = exp(ci[keep, 1]),
    conf_high  = exp(ci[keep, 2]),
    p_value    = coefs[keep, ncol(coefs)]
  )
}
