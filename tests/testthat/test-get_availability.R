
# Several of these functions are called from other tests, so for now we
# test only those functions in this file that are not tested indirectly.

test_that("get_tz() returns known time zones, as offsets or texts.", {
  # because the time zone offset changes with daylight savings time, two
  # different timezone offsets are possible

  offset <- get_tz('8454000')
  expect_lte(offset, -4)
  expect_gte(offset, -5)

  strng <- get_tz('8454000', .type = 'string')
  expect_equal(substr(strng, 1,8), 'Etc/GMT+')
  expect_lte(as.numeric(substr(strng,9,9)), 5)
  expect_gte(as.numeric(substr(strng,9,9)), 4)
})

test_that("get_tz() throws an error for unknown .type argumant.", {
  expect_error(get_tz('8454000', .type = 'banana'), 'should be')
})


