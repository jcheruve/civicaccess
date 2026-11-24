# civicaccess

<!-- badges: start -->
[![R-CMD-check](https://github.com/jcheruve/civicaccess/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/jcheruve/civicaccess/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

**civicaccess** is a small R package from [Nyaa Scientific Services](https://www.nyaahealth.org) for building census-tract measures of access to civic infrastructure and combining them into a composite index. It is written tidyverse-first (dplyr, purrr, rlang tidy evaluation, sf), with fully parameterized, documented, and tested functions.

All examples and tests use **synthetic data**. The package contains no client data, survey responses, or real organization records.

## What it does

| Step | Functions |
|---|---|
| Geocode addresses and assign them to tracts, with match-quality flags | `geocode_addresses()`, `flag_po_boxes()`, `points_from_lonlat()`, `assign_tracts()`, `geocode_qa_summary()` |
| Spatial access with enhanced two-step floating catchment area (E2SFCA) scores, Gaussian decay, and location-specific catchments | `e2sfca()`, `gaussian_decay()`, `binary_decay()` |
| Normalize indicators and build a dimension-based composite index | `normalize_indicator()`, `build_index()` |
| Monte Carlo rank uncertainty across normalization, aggregation, and weighting choices | `simulate_index_uncertainty()` |
| Survey-weighted helpers for validating an index against respondent data | `recode_binary()`, `svy_prop_by_group()`, `svy_logit_or()` |

## Installation

```r
# install.packages("remotes")
remotes::install_github("jcheruve/civicaccess")
```

## Example

```r
library(civicaccess)
toy <- toy_example()

# E2SFCA access for block groups, with urbanicity-specific catchments (meters)
access <- e2sfca(
  supply = toy$supply, demand = toy$demand,
  demand_id = bg_id, demand_pop = pop_18_29,
  catchment = catchment_m, supply_capacity = capacity
)

# Composite index from indicators grouped into dimensions
dims <- list(
  associational = c("nonprofit_access", "youth_org_access"),
  public        = c("library_access", "school_access"),
  connective    = "broadband_share"
)
idx <- build_index(toy$tract_indicators, dims, method = "percentile")

# How much do specification choices move each tract's rank?
unc <- simulate_index_uncertainty(toy$tract_indicators, dims, id = GEOID,
                                  n_draws = 200, seed = 2027)
```

See `vignette("civicaccess-workflow")` for the full walk-through.

## Design notes

* **Scales to national runs.** `e2sfca()` evaluates only supply–demand pairs within reach (`sf::st_is_within_distance()`), so memory grows with reachable pairs rather than with a full distance matrix. National runs process state chunks with supply buffered by the largest catchment.
* **Hand-checkable.** `binary_decay()` reproduces classic 2SFCA, and the test suite checks `e2sfca()` against a hand-calculated example.
* **Nothing dropped silently.** Records with missing coordinates are reported, points outside every tract keep `NA`, and unexpected survey codes become `NA` rather than being folded into a category.
* **Reproducible.** `simulate_index_uncertainty()` takes a seed and returns identical results on rerun.

## Development

```r
devtools::document()   # rebuild documentation from roxygen comments
devtools::test()       # run the testthat suite
devtools::check()      # full R CMD check
```

## License

MIT © 2026 Nyaa Health Services LLC
