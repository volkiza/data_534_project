library("testthat")
source("graph.R")

test_that("Check if function returns list", {
  airport_location() %>%
    expect_type("list")
  airport_location(airport = "JFK")%>%
    expect_type('list')
})



