library(tidyverse)
library(rvest)
library(janitor)

url<-'https://en.wikipedia.org/wiki/Macy%27s_Thanksgiving_Day_Parade'


get_table<-function(table_index, type){
  data = rvest::read_html(url)%>%
  rvest::html_elements("table.wikitable")%>%
  .[table_index]%>%
  rvest::html_table()%>%
  .[[1]]%>%
  janitor::clean_names()%>%
  rename(name=2)%>%
  mutate(name = gsub("\\s*\\([^\\)]+\\)", "", name))%>%
  separate_rows(name, sep = ", ")
  
  data$type<-type
  
  return(data)
  
  }

characters<-get_table(1, "character")
novelty<-get_table(2, "novelty")
heritage<-get_table(3, "heritage")
falloon<-get_table(5, "falloon")
balloonicle<-get_table(6, "balloonicle")
floalloonicle<-get_table(7, "floalloonicle")
performers<-get_table(8, "performer")

#combine balloon datasets 
df_balloons<-rbind(characters, novelty, heritage, falloon, balloonicle,floalloonicle)

#clean up performers
performers<-performers|>
  mutate(name = case_when(name=='Sesame Street cast' ~ 'The Cast of Sesame Street',
                          TRUE ~ name))

write.csv(df_balloons, "data/balloons.csv", row.names=F)
write.csv(performers, "data/performers.csv", row.names=F)