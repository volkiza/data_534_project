#install.packages("tibble")
#install.packages(tidyr)
#install.packages(stringr)
#install.packages(purrr)

source("getApi.R")
library(tibble)
library(tidyr)
library(stringr)
library(purrr)



lounges<-function(airport = "YVR", company = "LH", locaction = "", cabine=""){
  json_file<-getApi(apCode = airport, ac = company, loc = locaction, cab=cabine)
  data_raw<-enframe(unlist(json_file))
  rgx_split <- "\\."
  n_cols_max <-data_raw %>%pull(name) %>%str_split(rgx_split) %>%map_dbl(~length(.)) %>%max()
  data_raw<-data_raw%>%separate(name,into=paste0("name",1:n_cols_max),sep=rgx_split,fill="right")
  data_raw<-data_raw%>%filter((name1=='AirportResource' & name4=='AirportCode') |
                                (name1=='AirportResource' & name4=='CountryCode') |
                                (name1=='AirportResource' & name4=='CityCode') |
                                (name1=='AirportResource' & name6=='$3') |
                                (name1=='AirlineResource' & name6=='$') |
                                (name1=='LoungeResource' & name5=='Name' & name6=='$2') |
                                (name1=='LoungeResource' & name5=='Location' & name6=='$2') |
                                (name1=='LoungeResource' & name5=='OpeningHours' & name6=='$2') |
                                (name1=='LoungeResource' & name4=='Features' & name5=='NonSmokingLounge') |
                                (name1=='LoungeResource' & name4=='Features' & name5=='ShowerFacilities') |
                                (name1=='LoungeResource' & name4=='Features' & name5=='RelaxingRooms') |
                                (name1=='LoungeResource' & name4=='MagazinesAndNews' & name5=='International') )
  n_lounge<-data_raw%>%select(name1,name5,name6,value)%>%filter(name1=='LoungeResource'&name5=='Name' & name6=='$2')
  n_lounge<-nrow(as.data.frame(n_lounge))
  data_raw<-data_raw%>%select(name1,name4,name5,name6,value)%>%
    mutate(airport_code=if_else(name4=="AirportCode",value,NA_character_)) %>%
    mutate(city_code=if_else(name4=="CityCode",value,NA_character_)) %>%
    mutate(city=if_else(name1=='AirportResource' & name6=='$3',value,NA_character_)) %>%
    mutate(country_code=if_else(name4=="CountryCode",value,NA_character_)) %>%
    mutate(airline=if_else(name1=='AirlineResource'& name5=="Name",value,NA_character_)) %>%
    mutate(lounge_name=if_else(name1=='LoungeResource'&name5=="Name",value,NA_character_)) %>%
    mutate(lounge_location=if_else(name1=='LoungeResource'&name5=="Location",value,NA_character_)) %>%
    mutate(opening_hours=if_else(name1=='LoungeResource'&name5=="OpeningHours",value,NA_character_)) %>%
    mutate(smoking=if_else(name1=='LoungeResource'&name4=="Features"& name5=='NonSmokingLounge',value,NA_character_)) %>%
    mutate(shower=if_else(name1=='LoungeResource'&name4=="Features"& name5=='ShowerFacilities',value,NA_character_)) %>%
    mutate(relax_rooms=if_else(name1=='LoungeResource'&name4=="Features"& name5=='RelaxingRooms',value,NA_character_)) %>%
    mutate(magazines=if_else(name1=='LoungeResource'&name4=="MagazinesAndNews"& name5=='International',value,NA_character_))
  data_raw<-data_raw%>%fill(city_code,.direction="downup")%>%fill(city,.direction="downup")%>%fill(country_code,.direction="downup")%>%
    fill(airline,.direction="downup")%>%fill(airport_code,.direction="downup")
  data_raw<-data_raw%>%select(airport_code,city_code,city,country_code,airline,lounge_name,lounge_location,opening_hours,smoking,shower,relax_rooms,magazines)
  for(i in 1:nrow(data_raw)){
    data_raw<-lapply(data_raw,function(x){
      i1<-which(is.na(x))
      replace(x,i1,x[i1+n_lounge])})
  }
  data_raw<-(as.data.frame(data_raw))
  data_raw<-unique(data_raw%>%drop_na(lounge_name))
  return (data_raw)
}

lounges("YYZ")
lounges("FRA")
lounges("ZRH")
lounges()


#Call getApi function to get data about airport, company, cabine type:
json_file<-getApi("FRA")
#Json transformation to dataframe:
data_raw<-enframe(unlist(json_file))
data_raw
rgx_split <- "\\."
n_cols_max <-data_raw %>%pull(name) %>%str_split(rgx_split) %>%map_dbl(~length(.)) %>%max()
data_raw<-data_raw%>%separate(name,into=paste0("name",1:n_cols_max),sep=rgx_split,fill="right")
data_raw
write.csv(data_raw, file = "my_data.csv")
data_raw%>%filter(name1=='AirportResource' & name6=="$")


data_raw<-data_raw%>%filter((name1=='AirportResource' & name4=='AirportCode') |
                    (name1=='AirportResource' & name4=='CountryCode') |
                    (name1=='AirportResource' & name4=='CityCode') |
                    (name1=='AirportResource' & name6=='@LanguageCode3') |
                    (name1=='AirlineResource' & name6=='$') |
                    (name1=='LoungeResource' & name5=='Name' & name6=='$2') |
                    (name1=='LoungeResource' & name5=='Location' & name6=='$2') |
                    (name1=='LoungeResource' & name5=='OpeningHours' & name6=='$2') |
                    (name1=='LoungeResource' & name4=='Features' & name5=='NonSmokingLounge') |
                    (name1=='LoungeResource' & name4=='Features' & name5=='ShowerFacilities') |
                    (name1=='LoungeResource' & name4=='Features' & name5=='RelaxingRooms') |
                    (name1=='LoungeResource' & name4=='MagazinesAndNews' & name5=='International') )
data_raw
n_lounge<-data_raw%>%select(name1,name5,name6,value)%>%filter(name1=='LoungeResource'&name5=='Name' & name6=='$2')
n_lounge
n_lounge<-nrow(as.data.frame(n_lounge))
data_raw<-data_raw%>%select(name1,name4,name5,name6,value)%>%
  mutate(airport_code=if_else(name4=="AirportCode",value,NA_character_)) %>%
  mutate(city_code=if_else(name4=="CityCode",value,NA_character_)) %>%
  mutate(city=if_else(name1=='AirportResource' & name6=='$3',value,NA_character_)) %>%
  mutate(country_code=if_else(name4=="CountryCode",value,NA_character_)) %>%
  mutate(airline=if_else(name1=='AirlineResource'& name5=="Name",value,NA_character_)) %>%
  mutate(lounge_name=if_else(name1=='LoungeResource'&name5=="Name",value,NA_character_)) %>%
  mutate(lounge_location=if_else(name1=='LoungeResource'&name5=="Location",value,NA_character_)) %>%
  mutate(opening_hours=if_else(name1=='LoungeResource'&name5=="OpeningHours",value,NA_character_)) %>%
  mutate(smoking=if_else(name1=='LoungeResource'&name4=="Features"& name5=='NonSmokingLounge',value,NA_character_)) %>%
  mutate(shower=if_else(name1=='LoungeResource'&name4=="Features"& name5=='ShowerFacilities',value,NA_character_)) %>%
  mutate(relax_rooms=if_else(name1=='LoungeResource'&name4=="Features"& name5=='RelaxingRooms',value,NA_character_)) %>%
  mutate(magazines=if_else(name1=='LoungeResource'&name4=="MagazinesAndNews"& name5=='International',value,NA_character_))
data_raw<-data_raw%>%fill(city_code,.direction="downup")%>%fill(city,.direction="downup")%>%fill(country_code,.direction="downup")%>%
  fill(airline,.direction="downup")%>%fill(airport_code,.direction="downup")

data_raw<-data_raw%>%select(airport_code,city_code,city,country_code,airline,lounge_name,lounge_location,opening_hours,smoking,shower,relax_rooms,magazines)
data_raw
for(i in 1:nrow(data_raw)){
data_raw<-lapply(data_raw,function(x){
  i1<-which(is.na(x))
  replace(x,i1,x[i1+n_lounge])})
}

#data_raw<-(as.data.frame(data_raw))[1:n_lounge,]
as.data.frame(data_raw)%>% drop_na(lounge_name)

n_lounge
