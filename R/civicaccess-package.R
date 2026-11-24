#' civicaccess: Tract-Level Access Measures and Composite Indices for Civic Infrastructure
#'
#' Package-style, fully parameterized functions for building census-tract
#' measures of access to civic infrastructure:
#'
#' * **Geocoding and tract assignment** – [geocode_addresses()],
#'   [points_from_lonlat()], [assign_tracts()], [flag_po_boxes()], and
#'   [geocode_qa_summary()].
#' * **Spatial access** – [e2sfca()] with [gaussian_decay()] or
#'   [binary_decay()].
#' * **Composite index** – [normalize_indicator()], [build_index()], and
#'   [simulate_index_uncertainty()].
#' * **Survey-weighted validation helpers** – [recode_binary()],
#'   [svy_prop_by_group()], and [svy_logit_or()].
#'
#' All examples and tests use synthetic data from [toy_example()].
#'
#' @keywords internal
"_PACKAGE"

#' @importFrom rlang .data :=
#' @importFrom utils globalVariables
NULL

# Default column names used as data-masking defaults.
utils::globalVariables(c("long", "lat", "GEOID"))
