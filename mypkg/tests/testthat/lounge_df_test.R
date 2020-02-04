source("lounge_df.R")
test_that("Check if function returns list", {
  lk<-c("airport_code","city_code","city","country_code","airline","lounge_name", "lounge_location" ,"opening_hours","smoking","shower" ,"relax_rooms", "magazines")
  colnames(lounges())  %>%
    expect_equal(lk)
})


test_that("Check if function returns list", {
  lounges() %>%
    expect_type("list")
  lounges(airport = "JFK", company = "LH", locaction = "", cabine="")%>%
    expect_type('list')
})
