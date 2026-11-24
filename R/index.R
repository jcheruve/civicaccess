#' Normalize an indicator
#'
#' Rescales a raw indicator so that indicators on different scales can be
#' combined. Optional winsorizing limits the influence of extreme values.
#'
#' @param x Numeric vector.
#' @param method One of `"percentile"` (0–100 percentile rank), `"zscore"`
#'   (mean 0, standard deviation 1), or `"minmax"` (0–1).
#' @param winsorize Optional length-2 numeric vector of lower and upper
#'   quantiles (for example `c(0.01, 0.99)`) used to clip `x` before
#'   normalizing. `NULL` (default) applies no clipping.
#' @param direction `1` (default) if higher raw values mean more civic
#'   infrastructure, or `-1` to reverse the indicator.
#'
#' @return Numeric vector the same length as `x`. Missing values stay
#'   missing.
#' @family composite index
#' @export
#' @examples
#' normalize_indicator(c(1, 2, 3, 10), method = "percentile")
#' normalize_indicator(c(1, 2, 3, 10), method = "minmax", winsorize = c(0, 0.75))
normalize_indicator <- function(x, method = c("percentile", "zscore", "minmax"),
                                winsorize = NULL, direction = 1) {
  method <- rlang::arg_match(method)
  if (!is.numeric(x)) rlang::abort("`x` must be numeric.")
  if (!direction %in% c(1, -1)) rlang::abort("`direction` must be 1 or -1.")
  if (!is.null(winsorize)) {
    if (length(winsorize) != 2 || any(winsorize < 0 | winsorize > 1) ||
        winsorize[1] >= winsorize[2]) {
      rlang::abort("`winsorize` must be two increasing quantiles between 0 and 1.")
    }
    lims <- stats::quantile(x, winsorize, na.rm = TRUE, names = FALSE)
    x <- pmin(pmax(x, lims[1]), lims[2])
  }
  x <- x * direction
  switch(method,
    percentile = dplyr::percent_rank(x) * 100,
    zscore = {
      s <- stats::sd(x, na.rm = TRUE)
      if (is.na(s) || s == 0) rep(0, length(x)) else (x - mean(x, na.rm = TRUE)) / s
    },
    minmax = {
      r <- range(x, na.rm = TRUE)
      if (diff(r) == 0) rep(0, length(x)) else (x - r[1]) / diff(r)
    }
  )
}

#' Build a composite index from indicators grouped into dimensions
#'
#' Normalizes each indicator, averages indicators within each dimension, and
#' aggregates dimensions into a single index. Higher scores mean richer
#' civic infrastructure; rank 1 is the highest-scoring row.
#'
#' @param data A data frame (or `sf` object) with one row per tract.
#' @param dimensions A named list. Each element is a character vector of
#'   indicator columns in that dimension, for example
#'   `list(associational = c("nonprofit_access", "youth_org_access"),
#'   public = c("library_access", "school_access"))`.
#' @param method Normalization method passed to [normalize_indicator()].
#' @param aggregation `"arithmetic"` (default) or `"geometric"`. Geometric
#'   aggregation limits how much a strong dimension can offset a weak one and
#'   is computed on scores shifted by +1 so zeros are allowed. It cannot be
#'   used with `method = "zscore"`.
#' @param dim_weights Optional named numeric vector of dimension weights.
#'   Defaults to equal weights. Weights are rescaled to sum to 1.
#' @param winsorize Passed to [normalize_indicator()].
#' @param prefix Column prefix for the output index and rank.
#'
#' @return `data` with added columns `norm_<indicator>`, `dim_<dimension>`,
#'   `<prefix>_score`, and `<prefix>_rank`.
#' @family composite index
#' @export
#' @examples
#' toy <- toy_example()
#' build_index(toy$tract_indicators,
#'             dimensions = list(associational = c("nonprofit_access", "youth_org_access"),
#'                               public = c("library_access", "school_access"),
#'                               connective = "broadband_share"))
build_index <- function(data, dimensions, method = c("percentile", "zscore", "minmax"),
                        aggregation = c("arithmetic", "geometric"),
                        dim_weights = NULL, winsorize = NULL, prefix = "civic_index") {
  method <- rlang::arg_match(method)
  aggregation <- rlang::arg_match(aggregation)
  check_dimensions(data, dimensions)
  if (aggregation == "geometric" && method == "zscore") {
    rlang::abort("Geometric aggregation needs non-negative scores; use `method = \"percentile\"` or `\"minmax\"`.")
  }
  w <- resolve_weights(dim_weights, names(dimensions))
  indicators <- unique(unlist(dimensions, use.names = FALSE))

  out <- data |>
    dplyr::mutate(dplyr::across(
      dplyr::all_of(indicators),
      ~ normalize_indicator(.x, method = method, winsorize = winsorize),
      .names = "norm_{.col}"
    ))

  for (dim in names(dimensions)) {
    cols <- paste0("norm_", dimensions[[dim]])
    out[[paste0("dim_", dim)]] <- rowMeans(sf::st_drop_geometry(out)[cols], na.rm = FALSE)
  }

  dim_mat <- as.matrix(sf::st_drop_geometry(out)[paste0("dim_", names(dimensions))])
  score <- if (aggregation == "arithmetic") {
    as.vector(dim_mat %*% w)
  } else {
    exp(as.vector(log(dim_mat + 1) %*% w)) - 1
  }

  score_col <- paste0(prefix, "_score")
  rank_col <- paste0(prefix, "_rank")
  out[[score_col]] <- score
  out[[rank_col]] <- dplyr::min_rank(dplyr::desc(score))
  out
}

#' Monte Carlo rank uncertainty for index specification choices
#'
#' Rebuilds the index many times while drawing the normalization method,
#' aggregation rule, and dimension weights at random, then summarizes each
#' row's rank across draws. Wide intervals show tracts whose position depends
#' on modeling choices rather than on the data.
#'
#' @inheritParams build_index
#' @param id <[`data-masking`][rlang::args_data_masking]> Identifier column
#'   (for example, tract `GEOID`).
#' @param n_draws Number of Monte Carlo draws.
#' @param methods Normalization methods to sample from.
#' @param aggregations Aggregation rules to sample from. Geometric draws are
#'   skipped in favor of arithmetic when the sampled method is `"zscore"`.
#' @param weight_concentration Concentration of the symmetric Dirichlet
#'   distribution used to draw dimension weights. Larger values keep weights
#'   closer to equal.
#' @param probs Lower and upper quantiles for the rank interval.
#' @param seed Optional random seed for reproducibility.
#'
#' @return A tibble with the identifier, `median_rank`, `rank_low`,
#'   `rank_high`, and `rank_width`.
#' @family composite index
#' @export
#' @examples
#' toy <- toy_example()
#' simulate_index_uncertainty(
#'   toy$tract_indicators, id = GEOID,
#'   dimensions = list(associational = c("nonprofit_access", "youth_org_access"),
#'                     public = c("library_access", "school_access"),
#'                     connective = "broadband_share"),
#'   n_draws = 50, seed = 1
#' )
simulate_index_uncertainty <- function(data, dimensions, id, n_draws = 1000,
                                       methods = c("percentile", "zscore", "minmax"),
                                       aggregations = c("arithmetic", "geometric"),
                                       weight_concentration = 10,
                                       probs = c(0.05, 0.95), seed = NULL) {
  check_dimensions(data, dimensions)
  if (!is.null(seed)) set.seed(seed)
  df <- sf::st_drop_geometry(data)
  ids <- rlang::eval_tidy(rlang::enquo(id), df)
  k <- length(dimensions)

  ranks <- purrr::map(seq_len(n_draws), function(i) {
    m <- sample(methods, 1)
    a <- sample(aggregations, 1)
    if (m == "zscore") a <- "arithmetic"
    g <- stats::rgamma(k, shape = weight_concentration)
    w <- stats::setNames(g / sum(g), names(dimensions))
    build_index(df, dimensions, method = m, aggregation = a,
                dim_weights = w, prefix = "sim")$sim_rank
  })
  rank_mat <- do.call(cbind, ranks)

  tibble::tibble(
    !!rlang::as_name(rlang::enquo(id)) := ids,
    median_rank = apply(rank_mat, 1, stats::median),
    rank_low    = apply(rank_mat, 1, stats::quantile, probs = probs[1], names = FALSE),
    rank_high   = apply(rank_mat, 1, stats::quantile, probs = probs[2], names = FALSE)
  ) |>
    dplyr::mutate(rank_width = .data$rank_high - .data$rank_low)
}

check_dimensions <- function(data, dimensions, call = rlang::caller_env()) {
  if (!is.list(dimensions) || is.null(names(dimensions)) || any(names(dimensions) == "")) {
    rlang::abort("`dimensions` must be a named list of indicator column names.", call = call)
  }
  missing <- setdiff(unlist(dimensions, use.names = FALSE), names(data))
  if (length(missing) > 0) {
    rlang::abort(paste0("Indicator columns not found in `data`: ",
                        paste(missing, collapse = ", ")), call = call)
  }
  invisible(TRUE)
}

resolve_weights <- function(dim_weights, dims, call = rlang::caller_env()) {
  if (is.null(dim_weights)) return(stats::setNames(rep(1 / length(dims), length(dims)), dims))
  if (is.null(names(dim_weights)) || !setequal(names(dim_weights), dims)) {
    rlang::abort("`dim_weights` must be named with every dimension in `dimensions`.", call = call)
  }
  if (any(dim_weights < 0)) rlang::abort("`dim_weights` must be non-negative.", call = call)
  w <- dim_weights[dims]
  w / sum(w)
}
