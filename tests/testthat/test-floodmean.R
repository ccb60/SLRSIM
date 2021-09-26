test_that("floodmean() throws error with bad input", {
  expect_error(floodmean(.data = 'nothing', .dt = as.POSIXct((1:1200) * (60*60),
                                                              origin = "2021-01-01)"),
                          .wl = rnorm(1200),
                          .fldlvl = 1.97), 'inherits')

  expect_error( prov_tides %>% dplyr::filter(Year > 1990) %>%
                  floodmean(DateTime, MLLW,  .fldlvl= 'Bananas!'),
                'is.numeric')

  expect_error( prov_tides %>% dplyr::filter(Year > 1990) %>%
                  floodmean(DateTime, MLLW,  .fldlvl = c(1.97, 2.25)),
                'length')

  expect_error( prov_tides %>% dplyr::filter(Year > 1990) %>%
                  floodmean(dt, MLLW,  .fldlvl = 1.97),
                'inherits')

  expect_error( prov_tides %>% dplyr::filter(Year > 1990) %>%
                  floodmean(DateTime, MSL, .fldlvl = 1.97),
                "object 'MSL' not found")
})


test_that('floodmean() returns known values.', {

  test <- matrix(c(0.004496066, .000013,
                  1.524541027, 1.595068,
                  1.644061446, 1.690192), nrow = 3, byrow = TRUE)
  rownames(test) <-  c('Daily Probability', 'Flood Days', 'Scaled Flood Days')
  colnames(test) <- c('Mean', 'Var')
  attr(test ,"Years") <- 8


  expect_equal(prov_tides[prov_tides$Year >= 1940 & prov_tides$Year < 1950,] %>%
                 floodmean(DateTime, MLLW, 1.987), test, tolerance = 10^-5)
})
