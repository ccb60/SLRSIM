
portland_id <- 8418150
portland_epoch <- c(start = 1983, end = 2001)
portland_mllw <- 2.607
portland_HAT_MLLW <- 3.64

test_that("get_datums() retreives Portland MLLW", {
  p_ds<- get_datums(portland_id)
  expect_equal(p_ds$MLLW, portland_mllw)
})

test_that("get_datums() returns a named list of values", {
  p_ds <- get_datums(portland_id)
  expect_type(p_ds[[1]], "double")
  expect_named(p_ds)
  expect_type(p_ds, "list")
})

test_that("get_datums() raises error with faulty units", {
  expect_error(get_datums(portland_id, .units = 'MLLW'))
})

test_that("get_datums() raises error with faulty units", {
  expect_error(get_datums(portland_id, .units = 'boo'))
})

test_that("get_datums() raises error with faulty station ID", {
  expect_error(get_datums(-portland_id))
  expect_error(get_datums("portland_id"))
})

test_that("get_hat() can retreive Portland HAT", {
  p_hat<- get_hat(portland_id, .datum = 'MLLW')
  expect_equal(p_hat, portland_HAT_MLLW)
})

test_that("get_hat() raises error with faulty station ID", {
  expect_error(get_hat(-portland_id, .datum = 'MLLW'))
})

test_that("get_hat() raises error with faulty .datum", {
  expect_error(get_hat(portland_id, .datum = 'MLLLW'))
  expect_error(get_hat(portland_id, .datum = 12345))
})

test_that("get_hat() raises warning when .datum is missing.", {
  expect_warning(get_hat(portland_id))
})

test_that("get_epoch() can retreive Portland epoch", {
  p_e<- get_epoch(portland_id)
  expect_equal(p_e, portland_epoch)
})



