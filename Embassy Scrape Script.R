library(tidyverse)
library(rvest)
library(magrittr)
library(ggmap)


url <- "https://embassies.info"

countries <- url %>%
  read_html() %>%
  html_nodes("li") %>%
  html_nodes("a") %>% 
  html_attr('href')

search_url <- str_c(url, countries) 

embassies_and_consulates <- data.frame()  

for(site in search_url) { 
  temp <- site %>%  
    read_html() %>% 
    html_nodes("li") %>% 
    html_nodes("a") %>% 
    html_attr('href') %>% 
    as.data.frame()
  embassies_and_consulates <- bind_rows(embassies_and_consulates, temp)
}

#I did this because I needed to have a vector but the for loop above only worked as a data frame
embassies <- as.vector(embassies_and_consulates[[1]])

embassy_address_url <- str_c(url, embassies)

embassy_and_consulate_address <- data.frame()

for(site in embassy_address_url) { 
  try(
    temp <- site %>%  
    read_html() %>% 
    html_nodes("address.mrgn") %>% 
    html_text() %>% 
    as.data.frame()
    )
  embassy_and_consulate_address <- bind_rows(embassy_and_consulate_address, temp)
}

title <- data.frame()

for(site in embassy_address_url) { 
  try(
    temp <- site %>%  
      read_html() %>% 
      html_nodes("h1") %>% 
      html_text() %>% 
      as.data.frame()
  )
  title <- rbind(title, temp)
}


#clean up pulls ----
names(embassies_and_consulates) <- "embassy_url"
names(embassy_and_consulate_address) <- "address"

nothing <- embassies_and_consulates %>% 
  filter(str_detect(embassy_url, "/$"))

embassies_and_consulates <- embassies_and_consulates %>% 
  filter(!str_detect(embassy_url, "/$"))

title$. <- as.character(title$.)

title <- title %>% 
  filter(!str_detect(., "Embassies And Consulates Around The World"))

df <- cbind(embassies_and_consulates, embassy_and_consulate_address, title)

df$type <- word(df$., 1,2, sep=" ")
df$type <- str_replace_all(df$type, "of", "")
df$type <- as.factor(df$type)

type <- df %>% 
  group_by(type) %>% 
  summarize(total = n()) %>% 
  arrange(desc(total))

filter(str_detect(df$type, "British High"))









