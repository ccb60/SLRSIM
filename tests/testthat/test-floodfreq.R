test_that("floodcount() throws error with bad input", {
  expect_error(floodcount(.data = 'nothing', .dt = as.POSIXct((1:1200) * (60*60),
                                           origin = "2021-01-01)"),
                          .wl = rnorm(1200),
                          .fldlvl = 1.97), 'inherits')

  expect_error( prov_tides %>% dplyr::filter(Year > 1990) %>%
                  floodcount(DateTime, MLLW,  .fldlvl= 'Bananas!'),
                'is.numeric')

  expect_error( prov_tides %>% dplyr::filter(Year > 1990) %>%
                  floodcount(DateTime, MLLW,  .fldlvl = c(1.97, 2.25)),
                'length')

  expect_error( prov_tides %>% dplyr::filter(Year > 1990) %>%
                  floodcount(dt, MLLW,  .fldlvl = 1.97),
                'inherits')

  expect_error( prov_tides %>% dplyr::filter(Year > 1990) %>%
                  floodcount(DateTime, MSL, .fldlvl = 1.97),
                "object 'MSL' not found")
})


test_that('floodcount() returns known values.', {

  test <- c(days          =  2669,
            flood_days    = 12,
            daily_p_flood =  .004496066,
            floods_p_yr   =  1.6421)

  expect_equal(prov_tides[prov_tides$Year >= 1940 & prov_tides$Year < 1950,] %>%
    floodcount(DateTime, MLLW, 1.987), test, tolerance = 10^-4)
})
