library(MsdrCapstoneMPS)
context("Leaflet maps")

## Clean data
raw_noaa <- as.data.table(noaa_data)

test_that("Leaflet map", {
  g <- raw_noaa %>%
    eq_clean_data() %>%
    dplyr::filter(COUNTRY == "MEXICO" & lubridate::year(DATE) >= 2000) %>%
    eq_map(annot_col = "DATE")

  ## Plot is a leaflet map
  expect_equal(attributes(g)$package, "leaflet")
})

test_that("Leaflet map labels", {
  g <- raw_noaa %>%
    eq_clean_data() %>%
    eq_location_clean() %>%
    dplyr::filter(COUNTRY == "MEXICO" & lubridate::year(DATE) >= 2000) %>%
    dplyr::mutate(popup_text = eq_create_label(.))

  ## html label is built correctly
  expect_equal(g$popup_text[1], "<b>Location: </b>San Andres Tuxtla, Tuxtepec<br /> <b>Magnitude:</b> 5.9 <br />")
})
