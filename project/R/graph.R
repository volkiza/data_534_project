source("getApi.R")
source("lounge_df.R")
library(tibble)
library(tidyr)
library(stringr)
library(purrr)
library(ggplot2)
library(tidyverse)
library(ggmap)
library(plyr)
library(maps)
library(mapdata)
library(MUCflights)

airport_location <- function(airport = "YVR"){
  
  x <- getApi(apCode = airport)
  lat <- x[[1]]$AirportResource$Airports$Airport$Position$Coordinate$Latitude
  long <- x[[1]]$AirportResource$Airports$Airport$Position$Coordinate$Longitude
  df <- data.frame(
    a = long,
    b = lat)
  world <- map_data("world")
  p2<-ggplot(data = world) + geom_polygon(aes(x = long, y = lat, group = group), fill = NA, color = "red") + coord_fixed(1.3)
  p2<- p2+geom_point(data = df, aes(x = a, y = b), color = "steelblue", size = 3.5)
  print(p2)
  
}


# lounge_location <- function(airport = "YVR", company = "LH"){
#   
#   
#   x <- getApi(apCode = airport, ac = company)
#   num_lounge <- length(x[3][[1]]$Lounge)
#   print(num_lounge)
#   
# }
# 
# 
# lounge_location2 <- function(airport = "YVR", company = "LH"){
#   
#   
#   x <- lounges(airport = airport, company = company)
#   lounge <- x[6]
#   print(lounge)
#   
# }
# 
# airport_list = c("YVR", "ORD", "JFK", "ICN", "YLW", "ATL")
# for (i in airport_list) {
#   
#   lounge_location2(i)
#   
# }
# 
# 
# x <- getApi()
# for (i in x[3][[1]]$LoungeResource$Lounges$Lounge$Names$Name[2]){
#   print(i)
# }
  
