library(MsdrCapstoneMPS)
context("Earthquake timeline")

## Clean data
raw_noaa <- as.data.table(noaa_data)
clean_noaa <- eq_clean_data(raw_noaa)
clean_noaa <- eq_location_clean(clean_noaa)

## Set key to date
setkey(clean_noaa, DATE)

## Pull test set of 5 years
eq_subset <- clean_noaa[DATE >= "2005-01-01" & DATE <= "2010-12-31"]

## Subset to 5 countries
top_countries <- eq_subset[,.N,by = COUNTRY][order(-N)][1:5, COUNTRY]
eq_subset <- eq_subset[COUNTRY %in% top_countries]

## Set country to factor
eq_subset[, COUNTRY := as.factor(COUNTRY)]

## Set earthquake magnitude to numeric
eq_subset[, EQ_PRIMARY := as.numeric(EQ_PRIMARY)]

test_that("Timeline plot", {
  g <- ggplot(data = eq_subset, aes(x = DATE, y = COUNTRY)) + geom_timeline(xmin = "2005-01-01", xmax = "2010-12-31", aes(color = DEATHS,size = EQ_PRIMARY)) + labs(y = "")
  plot_data <- g$data

  ## Data is subsetting correctly
  expect_lte(as.numeric(as.Date("2005-01-01")), as.numeric(min(plot_data[, DATE])))
  expect_gte(as.numeric(as.Date("2010-12-31")), as.numeric(max(plot_data[, DATE])))

  ## Single layer for plot
  expect_equal(length(g$layers), 1)
})

test_that("Earthquake theme", {
  g <- ggplot(data = eq_subset, aes(x = DATE, y = COUNTRY)) + geom_timeline(xmin = "2005-01-01", xmax = "2010-12-31", aes(color = DEATHS,size = EQ_PRIMARY)) + labs(y = "") + theme_earthquake()

  ## Theme is a list of 61 items
  expect_equal(length(g[5]$theme), 61)
})

test_that("Timeline labels"), {
  g <- ggplot(data = eq_subset, aes(x = DATE, y = COUNTRY)) + geom_timeline(xmin = "2005-01-01", xmax = "2010-12-31", aes(color = DEATHS,size = EQ_PRIMARY)) + labs(y = "") + geom_timeline_labels(n_max = 20, aes(label = LOCATION_NAME, magnitude = EQ_PRIMARY))

  ## Adds a second layer for labels
  expect_equal(length(g$layers), 2)

  ## Label in second layer is LOCATION_NAME
  expect_equal(as.character(g[2]$layers[[2]]$mapping$label), "LOCATION_NAME")

  ## Magnitude is mapped to EQ_PRIMARY
  expect_equal(as.character(g[2]$layers[[2]]$mapping$magnitude), "EQ_PRIMARY")
}
