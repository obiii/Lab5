context("viser")

x <- getLimitedData(limit = 50)

test_that("API returns correct number of records", {
  expect_equal(nrow(getLimitedData(limit = 500)), 401)
  expect_equal(nrow(getLimitedData(limit = 10)), 9)
  
})

test_that("Invalid output.", {
  expect_error(getCrimeByID(x,99))
})
