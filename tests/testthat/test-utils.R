
test_that(".is_timezone() returns known values.", {
  expect_equal(.is_timezone(c('Etc/GMT+4', 'EST', 'EDT', 'America/New_York')),
               c(TRUE, TRUE, FALSE, TRUE))
})
print(getwd())

test_that(".lst_2_df() returns correctly translates known values", {
  the_df <- readRDS('test_obs_df.RDS')
  the_list <- readRDS('test_list.RDS')

  expect_equal(.lst_2_df(the_list), the_df)
})
