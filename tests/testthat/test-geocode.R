test_that("flag_po_boxes catches common PO box spellings only", {
  x <- c("PO Box 12, Lansing MI", "P.O. Box 9", "post office box 3",
         "12 Box Elder Rd", "100 Main St", NA)
  expect_equal(flag_po_boxes(x), c(TRUE, TRUE, TRUE, FALSE, FALSE, FALSE))
})

test_that("points_from_lonlat drops and reports missing coordinates", {
  df <- tibble::tibble(id = 1:3, long = c(-84.55, -84.48, NA), lat = c(42.73, 42.70, NA))
  expect_message(pts <- points_from_lonlat(df), "Dropped 1")
  expect_s3_class(pts, "sf")
  expect_equal(nrow(pts), 2)
})

test_that("assign_tracts joins points to tract polygons and keeps unmatched points", {
  toy <- toy_example()
  outside <- sf::st_as_sf(tibble::tibble(org_id = "FAR", x = 0, y = 0),
                          coords = c("x", "y"), crs = 32616)
  pts <- rbind(toy$supply[, "org_id"], outside)
  res <- assign_tracts(pts, toy$tracts, tract_id = GEOID)
  expect_equal(res$GEOID, c("26065000100", "26065000100", "26065000200", "26065000200", NA))
})

test_that("geocode_qa_summary computes match rates by group", {
  qa <- tibble::tibble(source = c("bmf", "bmf", "bmf", "imls"),
                       geo_matched = c(TRUE, TRUE, FALSE, TRUE),
                       po_box = c(FALSE, FALSE, TRUE, FALSE))
  res <- geocode_qa_summary(qa, source)
  expect_equal(res$match_rate, c(2 / 3, 1))
  expect_equal(res$n_po_box, c(1L, 0L))
})
