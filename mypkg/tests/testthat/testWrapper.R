source("getApi.r")

test_that("Check if proper token authorization", {
  expect_type(Authorization(), 'character')
})

test_that("Check if function returns list", {
  getApi() %>%
    expect_type("list")
  getApi(apCode = "JFK", ac = "LH", loc = "")%>%
    expect_type('list')
})

testthat::test_that("get API errors correctly", {
  testthat::expect_error(getApi(apCode = "123"))
})
