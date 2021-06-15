
test_that("season input validator", {
  expect_error(validate_season_input('2025-26'),
               "Input season is greater than the current season")
  expect_error(validate_season_input('1946-47'),
               "First available season is 1947-48")
  expect_error(validate_season_input('2019-21'),
               "please supply one season")
  expect_error(validate_season_input('202021'),
               "Season input missing hyphen. Season input in correct format: 'YYYY-YY'")
  
  
})

test_that("get current standings", {
  
  current_standings <- get_current_standings(season="2020-21")
  
  expect_equal(c(30, 81), dim(current_standings))
  

})

