
res_names = c('summary', 'details', 'settings', 'model')

prov_sum <- list(
  slope_ratio   = 2.8483,
  slope_old     = 1.8944,
  slope_old_err = 0.1474,
  slope_recent  = 5.3957,
  slope_recent_err = 0.5799 )

prov_details <- list(
  l_ratio   = 25.3025  ,
  p_value   = 0,
  delta_AIC = -23.3025,
  sample     = 868,
  recents    = 239
  )

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
  expect_error(slr_change(prov_meantrend, MSL_mm, MidDate,
                          .span = 20, .mode = 'clowns'))
  expect_error(slr_change(prov_meantrend, MSL_mm, MidDate,
                          .span = 20, t_fit = 42))
  expect_error(slr_change(prov_meantrend, MSL_mm, MidDate,
                          .span = 20, retain_model = 'if you like.'))
})

test_that("slr_change() sends messages with dates of 'recent' period", {
  expect_message(slr_change(prov_meantrend, MSL_mm, MidDate,
                            .span = 20, .mode = 'year'),
                 'The first date in the recent period')
  expect_message(slr_change(prov_meantrend, MSL_mm, MidDate,
                            .span = 20, .mode = 'year'),
                 'The last date in the recent period')
})




s <- slr_change(prov_meantrend, MSL_mm, MidDate, .span = 20, .mode = 'year')

test_that("slr_change() returns expected list components", {
    expect_true(all(names(s) %in% res_names))
})


test_that("slr_change() returns Providence results", {
  for (key in names(prov_sum)) {
    expect_lt(abs(s$summary[[key]] - prov_sum[[key]]), 0.001)
  }
})






