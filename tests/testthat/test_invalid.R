
context("Errors from invalid queries")

test_that("check that errors are thrown for bad response codes", {
  expect_error(check_status(100))
  expect_error(check_status(201))
  expect_error(check_status(300))
  expect_error(check_status(400))
  expect_error(check_status(401))
  expect_error(check_status(404))
  expect_error(check_status(500))
})

test_that("check for error with bad key", {
  expect_error(get_param_values(key='bad_key', param='short_desc'))
  expect_error(get_options(key='bad_key',
                           data_item='CORN, GRAIN - ACRES HARVESTED'))
  expect_error(search_data_items(key='bad_key',
                                 search_terms=c('corn')))
  expect_error(get_state_data(key='bad_key', year=2017,
                              data_item='CORN, GRAIN - ACRES HARVESTED',
                              fips='all'))
  expect_error(get_county_data(key='bad_key', year=2017,
                               data_item='CORN, GRAIN - ACRES HARVESTED',
                               fips='all'))
})

