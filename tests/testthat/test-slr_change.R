res_names = c('summary', 'details', 'settings', 'model')

prov_settings <- list(
  mode       = "year",
  span       = "20",
  cor_struct = "Order-based")

test_that("slr_change() throws errors given bad input.",{
   expect_error(slr_change('banana', MSL_mm, MidDate,
                           .span = 20))
   expect_error(slr_change(prov_meantrend, not_here, MidDate,
                           .span = 20))
   expect_error(slr_change(prov_meantrend, MSL_mm, not_here,
                           .span = 20))
   expect_error(slr_change(prov_meantrend, MSL_mm, MidDate,
                           .span = 'hiccup'))
   expect_error(slr_change(prov_meantrend, MDL_mm, MidDate,
                           .span = c(20,10 )))
   expect_error(slr_change(prov_meantrend, MSL_mm, MidDate,
                           .span = 20, .mode = 'clowns'))
   expect_error(slr_change(prov_meantrend, MSL_mm, MidDate,
                           .span = 20, t_fit = c(TRUE, TRUE)))
   expect_error(slr_change(prov_meantrend, MSL_mm, MidDate,
                           .span = 20, t_fit = 42))
   expect_error(slr_change(prov_meantrend, MSL_mm, MidDate,
                           .span = 20, retain_model = 'if you like.'))
   expect_error(slr_change(prov_meantrend, MSL_mm, MidDate,
                           .span = 20, retain_model = 17))
   expect_error(slr_change(prov_meantrend, not_here, MidDate,
                           .span = 20, retain_model = c(TRUE, FALSE)))
})

test_that("slr_change() throws errors for incompatible inputs.",{
  expect_error(slr_change(prov_meantrend, MSL_mm,
                          1:length(prov_meantrend$MSL_mm),
                          .mode = 'year'),
               '.mode == "year" requires .dt')

   expect_error(slr_change(prov_meantrend, MSL_mm, MidDate,
                           .mode = 'year', .span = 10.5),
                'If .mode == "year", .span must')
   expect_error(slr_change(prov_meantrend, MSL_mm,
                           1:length(prov_meantrend$MSL_mm),
                           .mode = 'duration', .span = 10),
                'If .mode == "duration", .span must')
   expect_error(slr_change(prov_meantrend, MSL_mm,
                           1:length(prov_meantrend$MSL_mm),
                            .mode = 'duration',
                           .span = as.difftime(104, units = 'weeks')),
                'If .mode == "duration", .dt must')
   expect_error(slr_change(prov_meantrend, MSL_mm,
                           1:length(prov_meantrend$MSL_mm),
                           .mode = 'count',
                           .span = 37.5),
                'If .mode == "count", .span must')
})

test_that("slr_change() sends messages with dates of 'recent' period", {
  expect_message(slr_change(prov_meantrend, MSL_mm, MidDate,
                            .span = 20, .mode = 'year'),
                 'The first date in the recent period')
  expect_message(slr_change(prov_meantrend, MSL_mm, MidDate,
                            .span = 20, .mode = 'year'),
                 'The last date in the recent period')
})

test_that("slr_change() returns Providence results.", {
  prov_sum <- list(
    slope_ratio   = 2.8483,
    slope_old     = 1.8944,
    slope_old_err = 0.1474,
    slope_recent  = 5.3957,
    slope_recent_err = 0.5799)

  prov_details <- list(
    l_ratio    = 25.0818,
    p_value    = 0,
    delta_AIC  = -23.0818,
    sample     = 868,
    recents    = 227)

  prov_set <- list(mode  = "year",
                   span  = "20",
                   cor_struct  = "Order-based")
  s <- slr_change(prov_meantrend, MSL_mm, MidDate, .span = 20, .mode = 'year')
  expect_true(all(names(s) %in% res_names))
  for (key in names(prov_sum)) {
    expect_lt(abs(s$summary[[key]] - prov_sum[[key]]), 0.001)
  }
  for (key in names(prov_details)) {
    expect_lt(abs(s$details[[key]] - prov_details[[key]]), 0.001)
  }
  for (key in names(prov_set)) {
    expect_equal(s$settings[[key]], prov_set[[key]])
  }
})

test_that("slr_change() returns numerical results with .mode = 'duration'", {
  prov_sum <- list(
    slope_ratio   = 2.7332,
    slope_old     = 0.0051,
    slope_old_err = 0.0004,
    slope_recent  = 0.0140,
    slope_recent_err =0.0015 )

  our_span <- round(as.difftime(240 * (365.2422/12), units = 'days'))- 5
  tmp <- prov_meantrend[1:(length(prov_meantrend$MidDate)-2),]
  s <- slr_change( tmp, MSL_mm, MidDate,
                .span = our_span, .mode = 'duration')

  for (key in names(prov_sum)) {
    expect_lt(abs(s$summary[[key]] - prov_sum[[key]]), 0.001)
  }
})

test_that("slr_change() returns results with .mode = 'count'", {
  s <- slr_change(prov_meantrend, MSL_mm, MidDate,
                   .span = 48, .mode = 'count')
  for (key in names(s$summary)) {
    expect_type(s$summary[[key]], 'double')
  }
})

test_that("slr_change() returns expected results with POSIXct.", {
  tmp <- prov_meantrend %>%
    dplyr::mutate(MidDate = as.POSIXct(MidDate))
  prov_sum <- list(
    'Estimate' =  2.413446,
    'Std_Err'  =  0.1207643,
    'P_Val'    =  2.314038e-73,
    'Lower_CI' =  2.405873,
    'Upper_CI' = 2.421019 )
  s = slr_slope(tmp, MSL_mm, MidDate, t_fit = FALSE)
  for (key in names(prov_sum)) {
    print(key)
    expect_lt(abs(s[[key]] - prov_sum[[key]]), 0.001)
    print('\n')}
})

 test_that("slr_change() sends message with POSIXct.", {
   tmp <- prov_meantrend %>%
     dplyr::mutate(MidDate = as.POSIXct(MidDate))
   expect_message(slr_slope(tmp, MSL_mm, MidDate,
                            .mode = 'year',  t_fit = FALSE),
                  'may be affected by rounding.')
})

test_that("slr_change() returns a model when retain_model = TRUE", {
  s <- slr_change(prov_meantrend, MSL_mm, MidDate, .span = 20, .mode = 'year',
                  retain_model = TRUE)
  expect_s3_class(s$model, 'gls')
})

