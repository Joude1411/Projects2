# Load packages

library(rvest)

library(stringr)

library(dplyr)

library(lubridate)

library(readr)



# Read web page

webpage <- read_html("https://www.nytimes.com/interactive/2017/06/23/opinion/trumps-lies.html")



# Extract records info

results <- webpage %>% html_nodes(".short-desc")



# Building the dataset

records <- vector("list", length = length(results))



for (i in seq_along(results)) {
  
  date <- str_c(results[i] %>% 
                  
                  html_nodes("strong") %>% 
                  
                  html_text(trim = TRUE), ', 2017')
  
  lie <- str_sub(xml_contents(results[i])[2] %>% html_text(trim = TRUE), 2, -2)
  
  explanation <- str_sub(results[i] %>% 
                           
                           html_nodes(".short-truth") %>% 
                           
                           html_text(trim = TRUE), 2, -2)
  
  url <- results[i] %>% html_nodes("a") %>% html_attr("href")
  
  records[[i]] <- data_frame(date = date, lie = lie, explanation = explanation, url = url)
  
}



df <- bind_rows(records)



# Transform to datetime format

df$date <- mdy(df$date)
View(df)

write_csv(df,"Covid-19 Datasets/Trump Lies.csv")



#Loading the rvest package
library('rvest')

#Specifying the url for desired website to be scraped
url <- 'http://www.imdb.com/search/title?count=100&release_date=2016,2016&title_type=feature'

#Reading the HTML code from the website
webpage <- read_html(url)
webpage
