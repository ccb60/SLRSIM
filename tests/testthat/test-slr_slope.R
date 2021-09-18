
# TODO:  Complete suite of tests for bad input data
test_that("Throws an error if first argument is not  a dataframe (or null).", {
  expect_error(slr_slope(list(date = as.Date(1:100, origin = "2021-01-01)",
                                             sl = rnorm(100)),
                              sl, date, t_fit = FALSE)))
})

test_that("Throws an error if t_fit is not logical.", {
  expect_error(slr_slope(prov_meantrend, MSL_mm, MidDate, t_fit = 'Bananas!'))
})


test_that("Throws an error if .mode is not one of the defined values.", {
  expect_error(slr_slope(prov_meantrend, MSL_mm, MidDate, .mode = "hiccup"))
})

test_that("Throws an error if .mode = 'year, but we don't have dates.", {
  mock_dates <- 1:length(prov_meantrend$MidDate)
  expect_error(slr_slope(prov_meantrend, MSL_mm, mock_dates, .mode = "year"))
})

prov_info <- list(
  Estimate = 2.4134,
  Std_Err = 0.1208,
  P_Val =  0.0000,
  Lower_CI = 2.4059,
  Upper_CI = 2.4210)

slope_info <- slr_slope(prov_meantrend, MSL_mm, MidDate)

test_that("slr_slope() returns Providence data with t_fit = FALSE.", {
  for (key in names(prov_info)) {
    expect_lt(abs(slope_info[[key]] - prov_info[[key]]), 0.001)
    }
})

test_that("sl_slope() returns attributes as expected.", {
  expect_equal(attr(slope_info, 'CI_P'), 0.95)
  expect_equal(attr(slope_info, "span"), 84)
  expect_equal(attr(slope_info, "start"),  1938)
  expect_equal(attr(slope_info, "end"), 2021)
  expect_equal(attr(slope_info, "cor_struct"), 'Order-based')
})

test_that("slr_slope() works with POSIXct data.", {
  tmp <- prov_meantrend %>%
    dplyr::mutate(MidDate = as.POSIXct(MidDate))
  slope_info_raw_POSIX <- slr_slope(tmp, MSL_mm, MidDate, t_fit = FALSE)

  for (key in names(prov_info)) {
    expect_lt(abs(slope_info_raw_POSIX[[key]] - prov_info[[key]]), 0.001)
  }
})

test_that("slr_slope() works in .mode = 'unscaled'", {
  prov_unsc_est <- 2.4132

  res <- prov_meantrend %>%
    dplyr::mutate(months = (Year - 1900) * 12 + (Month -1)) %>%
    slr_slope(MSL_mm, months, .mode = 'unscaled')
  res <- res[['Estimate']] * 12
 {
    expect_lt(abs(res - prov_unsc_est), 0.001)
  }
})

# The following is a fairly slow test.  We use a more approximate test to avoid
# having to put in another set of pre-calculated values, but this means we could
# have minor changes in values without noticing.
slope_info <- slr_slope(prov_meantrend, MSL_mm, MidDate, t_fit = TRUE)

test_that("slr_slope() returns Providence data with `tfit = TRUE.`", {
  for (key in names(prov_info)) {
    expect_lt(abs(slope_info[[key]] - prov_info[[key]]), 0.01)
  }
})

test_that("sl_slope() returns attributes as expected with t_fit = TRUE", {
  expect_equal(attr(slope_info, 'CI_P'), 0.95)
  expect_equal(attr(slope_info, "span"), 84)
  expect_equal(attr(slope_info, "start"),  1938)
  expect_equal(attr(slope_info, "end"), 2021)
  expect_equal(attr(slope_info, "cor_struct"), 'Time-based')
})
