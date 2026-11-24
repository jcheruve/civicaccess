#' Synthetic example data
#'
#' Builds a small, fully synthetic example in a projected coordinate system
#' (UTM zone 16N, meters) so that distances and access scores can be checked
#' by hand. No real organizations, people, or survey responses are included.
#'
#' @return A list with:
#'   * `demand` – `sf` points for six block groups with `bg_id`,
#'     `pop_18_29`, `urbanicity`, and `catchment_m`.
#'   * `supply` – `sf` points for four organizations with `org_id`, `type`,
#'     and `capacity`.
#'   * `tracts` – `sf` polygons for two tracts with `GEOID`.
#'   * `tract_indicators` – a tibble of five synthetic tract indicators for
#'     twelve tracts, for index examples.
#' @family example data
#' @export
#' @examples
#' toy <- toy_example()
#' toy$demand
toy_example <- function() {
  crs <- 32616

  demand <- sf::st_as_sf(
    tibble::tibble(
      bg_id       = sprintf("BG%02d", 1:6),
      pop_18_29   = c(400, 250, 300, 120, 60, 30),
      urbanicity  = c("urban", "urban", "urban", "suburban", "rural", "rural"),
      catchment_m = c(3000, 3000, 3000, 8000, 15000, 15000),
      x = c(700000, 701500, 703000, 708000, 716000, 724000),
      y = c(4730000, 4730500, 4729500, 4731000, 4733000, 4728000)
    ),
    coords = c("x", "y"), crs = crs
  )

  supply <- sf::st_as_sf(
    tibble::tibble(
      org_id   = sprintf("ORG%02d", 1:4),
      type     = c("library", "youth_org", "nonprofit", "library"),
      capacity = c(3, 1, 1, 2),
      x = c(700500, 702500, 709000, 723000),
      y = c(4730200, 4729800, 4731200, 4728500)
    ),
    coords = c("x", "y"), crs = crs
  )

  square <- function(x0, y0, size) {
    sf::st_polygon(list(matrix(c(x0, y0, x0 + size, y0, x0 + size, y0 + size,
                                 x0, y0 + size, x0, y0), ncol = 2, byrow = TRUE)))
  }
  tracts <- sf::st_sf(
    GEOID = c("26065000100", "26065000200"),
    geometry = sf::st_sfc(square(699000, 4728000, 5000), square(704000, 4728000, 22000),
                          crs = crs)
  )

  tract_indicators <- tibble::tibble(
    GEOID            = sprintf("260650%05d", seq(100, 1200, by = 100)),
    nonprofit_access = c(0.8, 1.2, 0.3, 2.1, 0.0, 0.6, 1.5, 0.9, 0.2, 1.1, 0.4, 3.0),
    youth_org_access = c(0.2, 0.5, 0.1, 0.9, 0.0, 0.3, 0.7, 0.4, 0.0, 0.6, 0.2, 1.4),
    library_access   = c(1.0, 0.8, 0.5, 1.3, 0.2, 0.9, 1.1, 0.7, 0.3, 1.0, 0.4, 1.6),
    school_access    = c(0.6, 0.9, 0.4, 1.0, 0.3, 0.5, 0.8, 0.6, 0.2, 0.7, 0.5, 1.2),
    broadband_share  = c(0.92, 0.88, 0.61, 0.97, 0.45, 0.80, 0.95, 0.83, 0.52, 0.90, 0.70, 0.99)
  )

  list(demand = demand, supply = supply, tracts = tracts, tract_indicators = tract_indicators)
}
