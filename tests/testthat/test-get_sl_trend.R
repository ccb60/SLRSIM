prov_info <- list(stationId = "8454000",
             stationName = "Providence, RI",
             affil = "US",
             latitude = 41.807098,
             longitude = -71.4012,
             trend = 2.4,
             trendError = 0.12,
             units = "mm/yr",
             startDate = "07/15/1938",
             endDate = "12/15/2020")

val <- get_sl_trend(8454000)
keys <- names(val)

test_that("Names of Sl Trend list match expectation.", {
  expect_equal(names(val), names(prov_info))
})

test_that("Retrieves Providence SLR rate record.", {
    expect_equal(val[keys], prov_info[keys])
})

test_that("Nonsense station name generates error.", {
  expect_error(get_sl_trend('Banana'), 'Station not found.')
})

  test_that("Nonsense .affil  generates error.", {
    expect_error(get_sl_trend(8454000, 'Boo!'),
                 'should be one of "us", "global", "all"')
})

test_that("Wrong .affil  generates error.", {
    expect_error(get_sl_trend(8454000, 'global'),
                 'Station not found.')
  })

# Add test for failed call to the URL?  I don't know how to force that error.
# Add test for finding multiple stations that match the search criteria
