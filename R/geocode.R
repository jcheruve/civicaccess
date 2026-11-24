#' Geocode addresses with match-quality flags
#'
#' A thin, parameterized wrapper around `tidygeocoder::geocode()` that adds a
#' `geo_matched` flag and a `po_box` flag so match quality can be reported
#' and tested downstream. The Census Bureau geocoder is the default because
#' it is free, has no usage key, and returns results suitable for tract
#' assignment.
#'
#' @param data A data frame of records to geocode.
#' @param address <[`data-masking`][rlang::args_data_masking]> Column holding a
#'   single-line address.
#' @param method Geocoding service passed to `tidygeocoder::geocode()`.
#'   Defaults to `"census"`.
#' @param ... Further arguments passed to `tidygeocoder::geocode()`.
#'
#' @return `data` with added `lat`, `long`, `geo_matched`, and `po_box`
#'   columns.
#' @family geocoding
#' @export
#' @examples
#' \dontrun{
#' orgs <- tibble::tibble(
#'   org = "Example Library",
#'   addr = "1600 Pennsylvania Ave NW, Washington, DC 20500"
#' )
#' geocode_addresses(orgs, address = addr)
#' }
geocode_addresses <- function(data, address, method = "census", ...) {
  rlang::check_installed("tidygeocoder", reason = "to geocode addresses.")
  address <- rlang::enquo(address)
  out <- tidygeocoder::geocode(data, address = !!address, method = method,
                               lat = "lat", long = "long", ...)
  out |>
    dplyr::mutate(
      geo_matched = !is.na(.data$lat) & !is.na(.data$long),
      po_box      = flag_po_boxes(!!address)
    )
}

#' Flag post office box addresses
#'
#' IRS Business Master File and similar administrative records often list a
#' mailing address rather than a physical location. PO boxes cannot be
#' placed accurately in a census tract, so they are flagged for exclusion or
#' sensitivity testing.
#'
#' @param x Character vector of addresses.
#' @return Logical vector, `TRUE` where `x` appears to be a PO box.
#' @family geocoding
#' @export
#' @examples
#' flag_po_boxes(c("PO Box 12, Lansing MI", "P.O. Box 9", "12 Box Elder Rd"))
flag_po_boxes <- function(x) {
  pattern <- "\\b(p\\.?\\s*o\\.?\\s*box|post\\s+office\\s+box|p\\.?\\s*o\\.?\\s*bx)\\b"
  !is.na(x) & grepl(pattern, x, ignore.case = TRUE, perl = TRUE)
}

#' Convert longitude and latitude columns to sf points
#'
#' Rows with missing coordinates are kept out of the `sf` object and reported
#' with a message, so the caller can account for every dropped record.
#'
#' @param data A data frame.
#' @param lon,lat <[`data-masking`][rlang::args_data_masking]> Longitude and
#'   latitude columns. Default to `long` and `lat`, as returned by
#'   [geocode_addresses()].
#' @param crs Coordinate reference system of the input. Defaults to WGS 84
#'   (EPSG:4326).
#'
#' @return An `sf` object of points.
#' @family geocoding
#' @export
#' @examples
#' df <- tibble::tibble(id = 1:3, long = c(-84.55, -84.48, NA), lat = c(42.73, 42.70, NA))
#' points_from_lonlat(df)
points_from_lonlat <- function(data, lon = long, lat = lat, crs = 4326) {
  lon_vals <- rlang::eval_tidy(rlang::enquo(lon), data)
  lat_vals <- rlang::eval_tidy(rlang::enquo(lat), data)
  keep <- !is.na(lon_vals) & !is.na(lat_vals)
  if (any(!keep)) {
    rlang::inform(sprintf("Dropped %d record(s) with missing coordinates.", sum(!keep)))
  }
  sf::st_as_sf(
    dplyr::mutate(data[keep, , drop = FALSE], .lon = lon_vals[keep], .lat = lat_vals[keep]),
    coords = c(".lon", ".lat"), crs = crs, remove = TRUE
  )
}

#' Assign points to census tracts
#'
#' Spatially joins point records to tract polygons. Points that fall outside
#' every tract (for example, water or a bad geocode) keep `NA` for the tract
#' identifier rather than being dropped.
#'
#' @param points An `sf` object of points.
#' @param tracts An `sf` object of tract polygons, for example from
#'   `tigris::tracts(state, year = 2020, cb = TRUE)`.
#' @param tract_id <[`tidy-select`][dplyr::dplyr_tidy_select]> Identifier
#'   column in `tracts`. Defaults to `GEOID`.
#'
#' @return `points` with the tract identifier column added.
#' @family geocoding
#' @export
#' @examples
#' toy <- toy_example()
#' assign_tracts(toy$supply, toy$tracts, tract_id = GEOID)
assign_tracts <- function(points, tracts, tract_id = GEOID) {
  check_sf_points(points, "points")
  if (!inherits(tracts, "sf")) rlang::abort("`tracts` must be an sf object.")
  tracts <- dplyr::select(tracts, {{ tract_id }})
  if (sf::st_crs(points) != sf::st_crs(tracts)) {
    tracts <- sf::st_transform(tracts, sf::st_crs(points))
  }
  sf::st_join(points, tracts, join = sf::st_within, left = TRUE)
}

#' Summarize geocoding match quality
#'
#' Produces the match-rate table used in a geocoding QA report.
#'
#' @param data Output of [geocode_addresses()] (or any data frame with
#'   logical `geo_matched` and `po_box` columns).
#' @param ... <[`data-masking`][rlang::args_data_masking]> Optional grouping
#'   columns, such as source or state.
#'
#' @return A tibble with counts and the match rate for each group.
#' @family geocoding
#' @export
#' @examples
#' qa <- tibble::tibble(
#'   source = c("bmf", "bmf", "imls"),
#'   geo_matched = c(TRUE, FALSE, TRUE),
#'   po_box = c(FALSE, TRUE, FALSE)
#' )
#' geocode_qa_summary(qa, source)
geocode_qa_summary <- function(data, ...) {
  data |>
    dplyr::group_by(...) |>
    dplyr::summarise(
      n_records  = dplyr::n(),
      n_matched  = sum(.data$geo_matched, na.rm = TRUE),
      n_po_box   = sum(.data$po_box, na.rm = TRUE),
      match_rate = .data$n_matched / .data$n_records,
      .groups = "drop"
    )
}
