context("viser")

x <- getLimitedData(limit = 50)

test_that("API returns correct number of records", {
  expect_equal(nrow(getLimitedData(limit = 500)), 401)
  expect_equal(nrow(getLimitedData(limit = 10)), 9)
  expect_equal(ncol(getLimitedData(limit = 10)), 5)
  
})

test_that("Invalid output.", {
  expect_error(getCrimeByID(x,99))
})


test_that("Invalid data", {
  expect_error(verify(length(colnames(x))) == 5)
  expect_error(verify(colnames(x)[1] == "crimeType"))
  expect_error(verify(colnames(x)[2] == "latitude"))
  expect_error(verify(colnames(x)[3] == "longitude"))
  expect_error(verify(colnames(x)[4] == "date"))
  expect_error(verify(getCrimeByID(2) == "JUVENILE"))
})
