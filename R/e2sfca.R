#' Enhanced two-step floating catchment area (E2SFCA) access scores
#'
#' Computes spatial access to supply locations (for example, nonprofits,
#' libraries, or colleges) for each demand location (for example, block-group
#' population-weighted centroids carrying the population ages 18–29).
#'
#' **Step 1.** For each supply location \eqn{j}, the supply-to-demand ratio is
#' \eqn{R_j = S_j / \sum_i P_i W(d_{ij}, d_{0,i})}, summing over demand
#' locations within reach.
#'
#' **Step 2.** For each demand location \eqn{i}, access is
#' \eqn{A_i = \sum_j R_j W(d_{ij}, d_{0,i})}.
#'
#' Catchments are defined on the demand side, so `catchment` may vary by
#' location (for example, wider catchments for rural block groups). Only
#' supply–demand pairs within the largest catchment are evaluated, using
#' [sf::st_is_within_distance()], so memory grows with the number of
#' reachable pairs rather than with the full distance matrix. For national
#' runs, call `e2sfca()` on state chunks with supply buffered by the largest
#' catchment so cross-border access is retained.
#'
#' @param supply An `sf` object of supply locations (points).
#' @param demand An `sf` object of demand locations (points).
#' @param demand_id <[`data-masking`][rlang::args_data_masking]> Column in
#'   `demand` that uniquely identifies each demand location.
#' @param demand_pop <[`data-masking`][rlang::args_data_masking]> Column in
#'   `demand` holding the population to be served.
#' @param catchment Either a single positive number, or a
#'   <[`data-masking`][rlang::args_data_masking]> column in `demand` giving a
#'   location-specific catchment. Units follow the CRS: meters for geographic
#'   (longitude/latitude) data, or the projected CRS's linear unit.
#' @param supply_capacity <[`data-masking`][rlang::args_data_masking]>
#'   Optional column in `supply` giving capacity (for example, enrollment or
#'   expenditure). If `NULL`, each supply location has capacity 1.
#' @param decay A function of `(d, d0)` returning weights, such as
#'   [gaussian_decay()] (default) or [binary_decay()].
#'
#' @return A tibble with one row per demand location and columns:
#'   the demand identifier, `access` (the E2SFCA score), and `n_supply`
#'   (number of supply locations with a positive weight).
#' @family spatial access
#' @export
#' @examples
#' toy <- toy_example()
#' e2sfca(toy$supply, toy$demand,
#'        demand_id = bg_id, demand_pop = pop_18_29,
#'        catchment = catchment_m, supply_capacity = capacity)
e2sfca <- function(supply, demand, demand_id, demand_pop, catchment,
                   supply_capacity = NULL, decay = gaussian_decay) {
  check_sf_points(supply, "supply")
  check_sf_points(demand, "demand")
  if (sf::st_crs(supply) != sf::st_crs(demand)) {
    rlang::abort("`supply` and `demand` must share the same CRS.")
  }
  decay <- rlang::as_function(decay)

  demand_df <- sf::st_drop_geometry(demand)
  supply_df <- sf::st_drop_geometry(supply)

  ids <- rlang::eval_tidy(rlang::enquo(demand_id), demand_df)
  pop <- rlang::eval_tidy(rlang::enquo(demand_pop), demand_df)
  if (anyDuplicated(ids)) rlang::abort("`demand_id` must uniquely identify demand rows.")
  if (!is.numeric(pop) || any(pop < 0, na.rm = TRUE)) {
    rlang::abort("`demand_pop` must be a non-negative numeric column.")
  }

  catch_quo <- rlang::enquo(catchment)
  d0 <- rlang::eval_tidy(catch_quo, demand_df)
  if (length(d0) == 1) d0 <- rep(d0, nrow(demand))
  if (!is.numeric(d0) || length(d0) != nrow(demand) || any(d0 <= 0, na.rm = TRUE)) {
    rlang::abort("`catchment` must be a positive number or a positive numeric column in `demand`.")
  }

  cap_quo <- rlang::enquo(supply_capacity)
  cap <- if (rlang::quo_is_null(cap_quo)) {
    rep(1, nrow(supply))
  } else {
    rlang::eval_tidy(cap_quo, supply_df)
  }

  # Reachable supply-demand pairs only (sparse), then exact distances.
  nb <- sf::st_is_within_distance(supply, demand, dist = max(d0, na.rm = TRUE))
  pairs <- tibble::tibble(
    s = rep(seq_along(nb), lengths(nb)),
    d = unlist(nb, use.names = FALSE)
  )

  if (nrow(pairs) == 0) {
    return(tibble::tibble(!!rlang::as_name(rlang::enquo(demand_id)) := ids,
                          access = 0, n_supply = 0L))
  }

  pairs <- pairs |>
    dplyr::mutate(
      dist = as.numeric(sf::st_distance(supply[.data$s, ], demand[.data$d, ],
                                        by_element = TRUE)),
      w    = decay(.data$dist, d0[.data$d]),
      pop  = pop[.data$d]
    )

  # Step 1: supply-to-demand ratio for each supply location.
  ratios <- pairs |>
    dplyr::summarise(denom = sum(.data$pop * .data$w, na.rm = TRUE), .by = "s") |>
    dplyr::mutate(ratio = dplyr::if_else(.data$denom > 0, cap[.data$s] / .data$denom, 0))

  # Step 2: sum decay-weighted ratios for each demand location.
  access <- pairs |>
    dplyr::left_join(ratios, by = "s") |>
    dplyr::summarise(
      access   = sum(.data$ratio * .data$w, na.rm = TRUE),
      n_supply = sum(.data$w > 0),
      .by = "d"
    )

  tibble::tibble(d = seq_len(nrow(demand)), id = ids) |>
    dplyr::left_join(access, by = "d") |>
    dplyr::mutate(
      access   = dplyr::coalesce(.data$access, 0),
      n_supply = dplyr::coalesce(.data$n_supply, 0L)
    ) |>
    dplyr::select(!!rlang::as_name(rlang::enquo(demand_id)) := "id", "access", "n_supply")
}

check_sf_points <- function(x, arg, call = rlang::caller_env()) {
  if (!inherits(x, "sf")) {
    rlang::abort(sprintf("`%s` must be an sf object.", arg), call = call)
  }
  types <- unique(as.character(sf::st_geometry_type(x)))
  if (!all(types %in% "POINT")) {
    rlang::abort(sprintf("`%s` must contain POINT geometries.", arg), call = call)
  }
  invisible(TRUE)
}
