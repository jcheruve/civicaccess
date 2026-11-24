#' Gaussian distance-decay weights for E2SFCA
#'
#' Computes the Gaussian decay function commonly used in enhanced two-step
#' floating catchment area (E2SFCA) models. Weights equal 1 at distance zero,
#' fall smoothly to 0 at the catchment threshold, and are 0 beyond it.
#'
#' \deqn{G(d, d_0) = \frac{e^{-\frac{1}{2}(d/d_0)^2} - e^{-\frac{1}{2}}}{1 - e^{-\frac{1}{2}}}
#'   \textrm{ for } d \le d_0, \textrm{ otherwise } 0}
#'
#' @param d Numeric vector of distances.
#' @param d0 Numeric catchment threshold(s), in the same units as `d`.
#'   Either length 1 or the same length as `d`.
#'
#' @return Numeric vector of weights in \[0, 1\], the same length as `d`.
#' @family spatial access
#' @export
#' @examples
#' gaussian_decay(c(0, 500, 1000, 1500), d0 = 1000)
gaussian_decay <- function(d, d0) {
  check_decay_inputs(d, d0)
  w <- (exp(-0.5 * (d / d0)^2) - exp(-0.5)) / (1 - exp(-0.5))
  ifelse(d <= d0, w, 0)
}

#' Binary (step) distance-decay weights
#'
#' Returns 1 for distances within the catchment and 0 otherwise. Using
#' `binary_decay()` in [e2sfca()] reproduces the classic two-step floating
#' catchment area (2SFCA) method, which is useful for hand-checking results.
#'
#' @inheritParams gaussian_decay
#' @return Numeric vector of 0/1 weights, the same length as `d`.
#' @family spatial access
#' @export
#' @examples
#' binary_decay(c(0, 999, 1000, 1001), d0 = 1000)
binary_decay <- function(d, d0) {
  check_decay_inputs(d, d0)
  as.numeric(d <= d0)
}

check_decay_inputs <- function(d, d0, call = rlang::caller_env()) {
  if (!is.numeric(d) || !is.numeric(d0)) {
    rlang::abort("`d` and `d0` must be numeric.", call = call)
  }
  if (any(d0 <= 0, na.rm = TRUE)) {
    rlang::abort("`d0` must be greater than 0.", call = call)
  }
  if (length(d0) != 1 && length(d0) != length(d)) {
    rlang::abort("`d0` must have length 1 or the same length as `d`.", call = call)
  }
  invisible(TRUE)
}
