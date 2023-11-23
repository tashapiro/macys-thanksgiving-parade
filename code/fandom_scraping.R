library(tidyverse)
library(rvest)
library(janitor)

base_url<-'https://macysthanksgiving.fandom.com'

url_giant<-'https://macysthanksgiving.fandom.com/wiki/Category:Giant_Balloons'
url_novelty<-'https://macysthanksgiving.fandom.com/wiki/Category:Novelty_Balloons'
url_balloonicles<-"https://macysthanksgiving.fandom.com/wiki/Category:Balloonicles"
url_floats<-'https://macysthanksgiving.fandom.com/wiki/Category:Floats'

get_balloons<-function(url, balloon_type){
  items<-rvest::read_html(url)%>%
    rvest::html_elements(".category-page__member-link")
  
  titles<-items%>%
    rvest::html_attr("title")
  
  links<-items%>%
    rvest::html_attr("href")
  
  balloons<-data.frame(title = titles, 
                       url = paste0(base_url, links),
                       type = balloon_type)
  
  return(balloons)
}



balloon_details<-function(url){
    html<-read_html(url)
    
    detail_labels<-html%>%
      html_elements(".pi-data-label")%>%
      html_text()
    
    detail_values<-html%>%
      html_elements(".pi-data-value")%>%
      html_text()
    
    data.frame(detail = detail_labels, value = detail_values)
}


all_details<-function(balloons){

  df_balloon_details<-data.frame()
  
  for(i in 1:nrow(balloons)){
    balloon = balloons$title[i]
    print(balloon)
    temp = balloon_details(balloons$url[i])
    temp$balloon = balloon
    df_balloon_details = rbind(df_balloon_details, temp)
  }
  
  balloons_cleaned = df_balloon_details %>%
    group_by(balloon) %>%
    mutate(version_num = cumsum(detail == "Years active"))
  
  pivoted_balloons<-balloons_cleaned|>
    tidyr::pivot_wider(id_cols = c(balloon, version_num), 
                       names_from = detail, 
                       values_from = value)|>
    janitor::clean_names()
  
  return(pivoted_balloons)
}


df_giant<-get_balloons(url_giant, "Giant Balloons")
df_novelty<-get_balloons(url_novelty, "Novelty Balloons")
df_balloonicles<-get_balloons(url_balloonicles, "Balloonicle")

df_giant_details<-all_details(df_giant)
df_giant_details$type<-"Giant Balloons"
df_novelty_details<-all_details(df_novelty)
df_novelty_details$type<-"Novelty Balloons"
df_balloonicles_details<-all_details(df_balloonicles)
df_balloonicles_details$type<-"Balloonicle"

df_balloons<-rbind(df_giant, df_novelty, df_balloonicles)|>rename(balloon=title)

df_balloon_details<-rbind(df_giant_details, df_novelty_details, df_balloonicles_details)

