library(MsdrCapstoneMPS)
context("Cleaning functions")

raw_noaa <- as.data.table(noaa_data)

test_that("Clean data is delivered correctly",{
  test_NOAA <- eq_clean_data(raw_noaa)
  expect_is(test_NOAA$DATE, "Date")
  expect_is(test_NOAA$LATITUDE, "numeric")
  expect_is(test_NOAA$LONGITUDE, "numeric")
})

test_that("eq_location_clean is stripping out the countries and fixing case",{
  test_NOAA <- eq_location_clean(raw_noaa)
  expect_equal(tail(test_NOAA[, LOCATION_NAME])[1],
               "Morotai")
})
