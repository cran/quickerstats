
context("Structure of objects returned from valid queries")

skip_if_no_auth <- function() {
  if (identical(Sys.getenv("NASS_KEY"), "")) {
    testthat::skip("No authentication available")
  }
}

test_that("get_param_values returns objects of correct size and type", {
  skip_if_no_auth()
  r <- get_param_values(key=Sys.getenv('NASS_KEY'), param='short_desc')
  expect_equal(class(r), 'character')
  expect_equal(class(r[1]), 'character')
  expect_equal(length(r[1]), 1)
  expect_true(length(r) > 20000 & length(r) < 40000)
})

test_that("get_options returns object of correct size and type", {
  skip_if_no_auth()
  r <- get_options(key=Sys.getenv('NASS_KEY'),
                   data_item='CORN, GRAIN - ACRES HARVESTED')
  expect_equal(class(r)[1], 'tbl_df')
  expect_equal(ncol(r), 4)
  expect_true(nrow(r) >= 1)
})

test_that("search_data_items returns objects of correct size and type", {
  skip_if_no_auth()
  r <- search_data_items(key=Sys.getenv('NASS_KEY'), search_terms=c('corn'))
  expect_equal(class(r), 'character')
  expect_equal(class(r[1]), 'character')
  expect_equal(length(r[1]), 1)
  expect_true(length(r) > 0)
})

test_that("get_state_data returns objects of correct size and type", {
  skip_if_no_auth()
  r1 <- get_state_data(key=Sys.getenv('NASS_KEY'), year=2017,
                       data_item='CORN, GRAIN - ACRES HARVESTED', fips='all')
  r2 <- get_state_data(key=Sys.getenv('NASS_KEY'), year=2017,
                       data_item='CORN, GRAIN - ACRES HARVESTED', fips='08')
  expect_equal(class(r1)[1], 'spec_tbl_df')
  expect_true(nrow(r1) > 20 & nrow(r1) < 55)
  expect_true(ncol(r1) > 30)
  expect_equal(class(r2)[1], 'spec_tbl_df')
  expect_equal(nrow(r2), 1)
  expect_true(ncol(r2) > 30)
})

test_that("get_county_data returns objects of correct size and type", {
  skip_if_no_auth()
  r1 <- get_county_data(key=Sys.getenv('NASS_KEY'), year=2017,
                        data_item='CORN, GRAIN - ACRES HARVESTED', fips='all')
  r2 <- get_county_data(key=Sys.getenv('NASS_KEY'), year=2017,
                        data_item='CORN, GRAIN - ACRES HARVESTED', fips='08')
  r3 <- get_county_data(key=Sys.getenv('NASS_KEY'), year=2017,
                        data_item='CORN, GRAIN - ACRES HARVESTED', fips='08069')
  expect_equal(class(r1)[1], 'spec_tbl_df')
  expect_true(nrow(r1) > 500 & nrow(r1) < 3800)
  expect_true(ncol(r1) > 30)
  expect_equal(class(r2)[1], 'spec_tbl_df')
  expect_true(nrow(r2) > 10)
  expect_true(ncol(r2) > 30)
  expect_equal(class(r3)[1], 'spec_tbl_df')
  expect_equal(nrow(r3), 1)
  expect_true(ncol(r3) > 30)
})


