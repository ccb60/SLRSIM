test_that("call_api() finds known observed values, with attributes", {
  .begin_date <- '20200801'
  .end_date   <- '20200802'
  .station    <- '8454000'  # Providence, RI
  test_df <- readRDS('test_obs_df.RDS')

  res <- head(call_api(.station,
                .begin_date,
                .end_date,
                .which ='observed'), 5)
  expect_equal(attr(res, 'datum'), 'MSL')
  attr(res, 'datum') <- NULL
  expect_equal(attr(res, 'station'), '8454000')
  attr(res, 'station') <- NULL
  expect_equal(attr(res, 'timefmt'), 'gmt')
  attr(res, 'timefmt') <- NULL
  expect_equal(attr(res, 'which'), 'observed')
  attr(res, 'which') <- NULL
  expect_equal(attr(res, 'names'), c('datetime', 'water_level'))
  attr(res, 'url') <- NULL

  expect_equal(res, test_df)
})


test_that("call_api() finds known predicted values, with attributes.", {
  .begin_date <- as.Date('2020-08-01')
  .end_date   <- as.Date('2020-08-02')
  .station    <- '8454000'  # Providence, RI
  test_df <- readRDS('test_pred_df.RDS')

  res <- head(call_api(.station,
                       .begin_date,
                       .end_date,
                       .which ='observed'), 5)
  expect_equal(attr(res, 'datum'), 'MSL')
  attr(res, 'datum') <- NULL
  expect_equal(attr(res, 'station'), '8454000')
  attr(res, 'station') <- NULL
  expect_equal(attr(res, 'timefmt'), 'gmt')
  attr(res, 'timefmt') <- NULL
  expect_equal(attr(res, 'which'), 'observed')
  attr(res, 'which') <- NULL
  expect_equal(attr(res, 'names'), c('datetime', 'water_level'))
  attr(res, 'url') <- NULL

  expect_equal(res, test_df)
})

test_that("call_api() throws warnings and errors.", {
  .begin_date <- '20200801'
  .end_date   <- '20200805'
  .station    <- '8454000'  # Providence, RI
  test_df <- readRDS('test_pred_df.RDS')

  expect_warning(call_api(.station,
                       .begin_date,
                       .end_date), 'not specified')
  expect_error(call_api(.station,
                        'error',
                        .end_date,
                        .which ='observed'), 'Malformed')
  expect_error(call_api(.station,
                        .begin_date,
                        'error',
                        .which ='observed'), 'Malformed')
  expect_error(call_api(.station,
                        '17760704',
                        .end_date,
                        .which ='observed'), 'requires dates')
  expect_error(call_api(.station,
                        .begin_date,
                        '17760704',
                        .which ='observed'), 'requires dates')
  expect_error(call_api('1010101',
                        .begin_date,
                        .end_date,
                        .which ='observed'), 'API')


})

test_that("retrieve_data() returns observed data as expected.", {
  .begin_yr <- 2019
  .end_yr   <- 2020
  .station    <- '8454000'  # Providence, RI

  res <- retrieve_data(.station,
                       .begin_yr,
                       .end_yr,
                       .which ='observed')

  expect_equal(format(min(res$datetime), format = '%Y%m'), '201901')
  expect_equal(format(max(res$datetime), format = '%Y%m'), '202012')
  expect_lte(nrow(res), (365 + 366) * 24)

  expect_equal(attr(res, 'datum'), 'MSL')
  expect_equal(attr(res, 'station'), '8454000')
  expect_equal(attr(res, 'timefmt'), 'gmt')
  expect_equal(attr(res, 'which'), 'observed')
  expect_equal(attr(res, 'names'), c('datetime', 'water_level'))
})

test_that("retrieve_data() returns predicted data as expected.", {
  .begin_yr <- 2019
  .end_yr   <- 2020
  .station    <- '8454000'  # Providence, RI

  res <- retrieve_data(.station,
                       .begin_yr,
                       .end_yr,
                       .which ='predicted')

  expect_equal(format(min(res$datetime), format = '%Y%m'), '201901')
  expect_equal(format(max(res$datetime), format = '%Y%m'), '202012')
  expect_lte(nrow(res), (365 + 366) * 24)

  expect_equal(attr(res, 'datum'), 'MSL')
  expect_equal(attr(res, 'station'), '8454000')
  expect_equal(attr(res, 'timefmt'), 'gmt')
  expect_equal(attr(res, 'which'), 'predicted')
  expect_equal(attr(res, 'names'), c('datetime', 'predicted'))
})

test_that("retrieve_data() throws errors,  warnings and messages.", {
  .station    <- '8454000'  # Providence, RI
  expect_error(retrieve_data(.station,
                             2020,
                             2021), 'Must specify')
  expect_error(retrieve_data(.station,
                             'twenty twenty',
                             2021,
                             .which ='observed'), 'must be numeric')
  expect_error(retrieve_data(.station,
                             2020.5,
                             2021,
                             .which ='observed'), 'must be integers')
  expect_message(retrieve_data(.station,
                               1937,
                               1939,
                               .which ='observed'), 'available')
  expect_message(retrieve_data(.station,
                             2021,
                             2025,
                             .which ='observed'), 'available')
  expect_error(retrieve_data(.station,
                               2020,
                               2021,
                               .which ='observed', .tz = 'Whenever'),
               'recognized')
})
