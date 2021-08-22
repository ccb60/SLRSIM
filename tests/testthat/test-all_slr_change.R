test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

test_that("all_slr_change() produces expected results with .mode = 'year'.", {
all_slopes <- all_slr_change(prov_meantrend, MSL_mm, MidDate, .span = 20,
                             .interval = 1, .mode = 'year')

expect_equal(all_slopes[[1]], 1)
expect_equal(all_slopes[[2]], 50)
expect_equal(names(all_slopes), c("n_higher", "n_slopes", "df" ))

all_slopes <- all_slr_change(prov_meantrend, MSL_mm, MidDate, .span = 15,
                             .interval = 1, .mode = 'year')
expect_equal(all_slopes[[1]], 9L)
expect_equal(all_slopes[[2]], 54L)
})

test_that("all_slr_change() metadata attributes are correct in .mode = 'year'.", {
  all_slopes <- all_slr_change(prov_meantrend, MSL_mm, MidDate, .span = 20,
                               .interval = 1, .mode = 'year')
  atr <- attributes(all_slopes)[2:5]
  expect_equal(names(atr),
               c("mode", "span", "interval", "cor_struct"))
  expect_equal(atr$mode, 'year')
  expect_equal(atr$span, 20)
  expect_equal(atr$interval, 1)
  expect_equal(atr$cor_struct,'Order-based')
})

test_that("all_slr_change() works in .mode = 'duration'.", {
all_slopes <- all_slr_change(prov_meantrend, MSL_mm, MidDate,
                              .span = as.difftime(3650, units = 'days'),
               .interval = as.difftime(180, units = 'days'),
               .mode = 'duration')
expect_equal(all_slopes[[1]], 41)
expect_equal(all_slopes[[2]], 119)
atr <- attributes(all_slopes)[2:5]
expect_equal(names(atr),
             c("mode", "span", "interval", "cor_struct"))
expect_equal(atr$mode, 'duration')
expect_equal(atr$span, as.difftime(3650, units = 'days'))
expect_equal(atr$interval, as.difftime(180, units = 'days'))
expect_equal(atr$cor_struct,'Order-based')
})

## By Number of Samples
test_that("all_slr_change() works in .mode = 'count'.", {
  all_slopes <- all_slr_change(prov_meantrend, MSL_mm, MidDate,
                               .span = 200,
                               .interval = 25,
                               .mode = 'count')
  expect_equal(all_slopes[[1]], 6)
  expect_equal(all_slopes[[2]], 27)
})



test_that("all_slr_change() raises an error given crazy data.", {
  expect_error(all_slr_change('banana', MSL_mm, MidDate,
                              .span = 20,
               'is.data.frame(.data)'))
  expect_error(all_slr_change(prov_meantrend, not_here, MidDate,
                          .span = 20),
               'not found')
  expect_error(all_slr_change(prov_meantrend, MSL_mm, not_here,
                          .span = 20),
               'not found')
  expect_error(all_slr_change(prov_meantrend, MSL_mm, MidDate,
                          .span = 'hiccup',
               'is.numeric(.span)'))
  expect_error(all_slr_change(prov_meantrend, MDL_mm, MidDate,
                          .span = c(20,10)),
               '.span')
  expect_error(all_slr_change(prov_meantrend, MSL_mm, MidDate,
                          .span = 20, .mode = 'clowns'),
               "should be one of")
  expect_error(all_slr_change(prov_meantrend, MSL_mm, MidDate,
                          .span = 20, t_fit = c(TRUE, TRUE)),
               'logical')
  expect_error(all_slr_change(prov_meantrend, MSL_mm, MidDate,
                          .span = 20, t_fit = 42),
               'logical')
})


test_that("all_slr_change() throws errors for incompatible inputs.",{
  expect_error(all_slr_change(prov_meantrend, MSL_mm,
                          1:length(prov_meantrend$MSL_mm,
                                   .mode = 'year'),
                          '.mode == "year" requires .dt'))
  expect_error(all_slr_change(prov_meantrend, MSL_mm,
                              as.numeric(MidDate), .span = 20,
                              .interval = 1, .mode = 'year'),
               'requires .dt')
  expect_error(all_slr_change(prov_meantrend, MSL_mm, MidDate,
                          .mode = 'year', .span = 10.5),
               'integer-valued .span')
  expect_error(all_slr_change(prov_meantrend, MSL_mm, MidDate,
                              .mode = 'year', .interval = 20.5),
               'integer-valued .interval')
  expect_error(all_slr_change(prov_meantrend, MSL_mm,
                              as.numeric(MidDate),
                              .span = as.difftime(3650, units = 'days'),
                              .interval = as.difftime(180, units = 'days'),
                              .mode = 'duration'),
               'requires .dt')
  expect_error(all_slr_change(prov_meantrend, MSL_mm, MidDate,
                              .span = 3650,
                              .interval = as.difftime(180, units = 'days'),
                              .mode = 'duration'),
               'requires .span')
  expect_error(all_slr_change(prov_meantrend, MSL_mm, MidDate,
                              .span = as.difftime(3650, units = 'days'),
                              .interval = 180,
                              .mode = 'duration'),
               'requires .interval')

  expect_error(all_slr_change(prov_meantrend, MSL_mm, MidDate,
                              .span = 200.5,
                               .interval = 25,
                               .mode = 'count'))
  expect_error(all_slr_change(prov_meantrend, MSL_mm, MidDate,
                              .span = 200,
                              .interval = 25.5,
                              .mode = 'count'))
})
