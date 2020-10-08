
context("Search function operating properly")

skip_if_no_auth <- function() {
  if (identical(Sys.getenv("NASS_KEY"), "")) {
    testthat::skip("No authentication available")
  }
}

test_that("search function operating properly", {
  skip_if_no_auth()
  r1 <- search_data_items(key=Sys.getenv('NASS_KEY'),
                          search_terms=c('corn'))
  r2 <- search_data_items(key=Sys.getenv('NASS_KEY'),
                          search_terms=c('corn', 'harvested'))
  r3 <- search_data_items(key=Sys.getenv('NASS_KEY'),
                          search_terms=c('corn', 'harvested'),
                          exclude=c('grain', 'silage'))
  expect_true(length(r3) < length(r2))
  expect_true(length(r2) < length(r1))
  count <- 0
  for (r in r2) {
    count <- count + (grepl('CORN', r) & grepl('HARVESTED', r))
  }
  expect_equal(count, length(r2))
  count <- 0
  for (r in r3) {
    count <- count + (grepl('GRAIN', r) | grepl('SILAGE', r))
  }
  expect_equal(count, 0)
})

test_that("check get_options handles no options", {
  skip_if_no_auth()
  expect_true(is.null(get_options(key=Sys.getenv('NASS_KEY'),
                                  data_item = 'CORN - ACRES HARVESTED')))
})
