library(dplyr)
library(tidyverse)
library(ggplot2)
library(reshape2)
library(mapproj)
library(maps)



incarceration <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")
wa_aapi_incarcerations <- incarceration %>%
  filter(state == "WA", year >= "1990")%>%
  summarize(year, state, county_name, total_pop_15to64, aapi_pop_15to64, fips)

  
counties_2018 <- wa_aapi_incarcerations%>%
  filter(year=="2018")%>%
  summarize(county_name)
counties_1990 <- wa_aapi_incarcerations %>%
  filter(year=="1990")%>%
  summarize(county_name)
avgin2018 <- wa_aapi_incarcerations %>% #1st stat
  filter(year=="2018")%>%
  summarize(avg2018 = sum(aapi_pop_15to64)/nrow(counties_2018))
avgin1990 <- wa_aapi_incarcerations %>% #2nd stat
  filter(year=="1990")%>%
  summarize(avg1990 = sum(aapi_pop_15to64)/nrow(counties_1990))
percentin2018 <- wa_aapi_incarcerations %>% #3rd stat
  filter(year == "2018")%>%
  summarize(avg = sum(aapi_pop_15to64)/sum(total_pop_15to64)*100)
percentin1990 <- wa_aapi_incarcerations %>% #4th stat
  filter(year == "1990")%>%
  summarize(avg = sum(aapi_pop_15to64)/sum(total_pop_15to64)*100)
highest_num <- wa_aapi_incarcerations %>%   #5th one
  filter(aapi_pop_15to64 == max(aapi_pop_15to64))
lowest_num <- wa_aapi_incarcerations%>%
  filter(aapi_pop_15to64 == min(aapi_pop_15to64))

#chart1 prep
total_aapi_year <- wa_aapi_incarcerations %>%
  group_by(year)%>%
  summarize(total_pop_15to64 = sum(total_pop_15to64), total_aapi_15to64 = sum(aapi_pop_15to64))
yearly_dataWA <- total_aapi_year %>% #flipping
  gather(key= "Aapi_vs_total", value = "Population", -year)


#chart2 prep made 2 charts with same vari just changed some prams on one and took out large dataset that made chart extend
data_aapi <- wa_aapi_incarcerations %>% 
  select(aapi_pop_15to64, total_pop_15to64)

  
data_aapiver2 <- wa_aapi_incarcerations %>% 
  filter(county_name != "King County")%>%
  select(aapi_pop_15to64, total_pop_15to64)


#map
countiesWA <- wa_aapi_incarcerations %>%
  filter(state == "WA", year == "2018")%>%
  summarize(year, state, county_name, fips, aapi_pop_15to64,)

map <- map_data("county")%>%
unite(polyname, region, subregion, sep = ",")%>%
left_join(county.fips, by="polyname")

map_data <- map %>%
  left_join(countiesWA, by="fips")%>%
  filter(state == "WA")





