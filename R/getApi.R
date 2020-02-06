library(dplyr)
library(httr)
library(jsonlite)

# Function takes in token and converts it into proper form for the website to authorize, to access the 
#Lufthansa API

Authorization <- function(t="qguqjjnebe4fv58x347ttaqt"){
  token <- t
  return( paste("Bearer", token) )
}

# Wrapper function to get the API data, which is then converted to JSON for better readibilty.
# The content is then return in a list format
# Inputs are the airport code, the airline, the location of a city by code, and cabin class you are flying in

getApi <- function(apCode = "JFK", ac = "LH", loc = "", cab=""){
  

  # check input class. should be class = characeter.
  if (class(apCode) != "character") {
    stop("Problem with the API input : ", apCode, " -Input parameter class for airport should be 'character'. ie. 'JFK'")
  }
  if(loc == ""){
    loc = apCode
  }
  url_airport_api <- "https://api.lufthansa.com/v1/references/airports/"
  url_airport_api_airport <- paste0(url_airport_api, apCode)
  
  url_airline_api <- "https://api.lufthansa.com/v1/mds-references/airlines/"
  url_airline_api_airport <- paste0(url_airline_api, ac)
  
  url_lounge_api <- "https://api.lufthansa.com/v1/offers/lounges/"
  url_lounge_api_airport <- paste0(url_lounge_api, loc,"?cabinClass=",cab)
  
  # Getting the content from the Airport Resources API
  received_content1 <- httr::GET(url = url_airport_api_airport,
                                config = httr::add_headers(Authorization = Authorization()))
  received_content2 <- httr::GET(url = url_airline_api_airport,
                                 config = httr::add_headers(Authorization = Authorization()))
  received_content3 <- httr::GET(url = url_lounge_api_airport,
                                 config = httr::add_headers(Authorization = Authorization()))
  
  if (received_content1$status_code != 200) {
    stop("Problem with calling the airport API - response: ", content(received_content1))
  }
  if (received_content2$status_code != 200) {
    stop("Problem with calling the airline API - response: ", content(received_content1))
  }
  if (received_content3$status_code != 200) {
    stop("Problem with calling the lounge API - response: ", content(received_content1))
  }
  
  airport_content <- httr::content(received_content1, "text",encoding = "UTF-8")
  airline_content <- httr::content(received_content2, "text",encoding = "UTF-8")
  lounge_content <- httr::content(received_content3, "text",encoding = "UTF-8")
  ac<- airport_content %>% fromJSON
  alc<-airline_content %>% fromJSON
  lc<-lounge_content %>% fromJSON
  newlist<-list(ac,alc,lc)
  return(newlist)
  
}
