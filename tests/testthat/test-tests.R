library(testthat)
library(mapdata)
library(farsdata)
test_that('Test that mapping works', {
  expect_error(fars_map_state(44, 2016))
})
