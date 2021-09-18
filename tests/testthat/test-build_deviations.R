test_that("build_deviations() returns known observed values, with attributes", {
  .begin_yr <- 2020
  .end_yr   <- 2020
  .station    <- '8454000'

  test_df <- readRDS('test_build_deviations.RDS')

  res <- head(build_deviations(.station,
                          .begin_yr,
                          .end_yr), 10)
  expect_equal(attr(res, 'station'), '8454000')
  expect_equal(attr(res, 'datum'), 'MSL')
  expect_equal(attr(res, 'units'), 'metric')
  expect_equal(attr(res, 'timefmt'), 'gmt')
  expect_equal(attr(res, 'tz'), 'UTC')
  expect_equal(attr(res, 'names'), c('datetime', 'water_level', 'deviations'))
  expect_equal(res, test_df)
})
