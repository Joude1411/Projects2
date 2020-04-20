
getwd()
library(httr)

#Function to connect with APIs respectfully
get_pageviews <- function(article_title){
  url <- paste(
    "https://wikimedia.org/api/rest_v1/metrics/pageviews/per-article/en.wikipedia/all-access/all-agents", 
    article_title, 
    "daily/2015100100/2015103100", 
    sep = "/"
  )   
  response <- GET(url, user_agent("joude1411@gmail.com This is a test")) 
  # Is there an HTTP error?
  if(http_error(response)){ 
    # Throw an R error
    stop("the request failed") 
  }
  # Return the response's content
  content(response)
}

get_pageviews("Hadley Wickham")


#function to retrieve the format of a response
rev_history <- function(article_title){
if (format == "json"){
  resp <- readRDS("had_rev_json.rds")
} else if (format == "xml"){
  resp <- readRDS("had_rev_xml.rds")
} else {
  stop('Invalid format supplied, try "json" or "xml"')
}
resp  
}


# Load rlist
#install.packages("rlist")
library(rlist)

# Examine output of this code
str(content(resp_json), max.level = 4)

# Store revision list
revs <- content(resp_json)$query$pages$`41916270`$revisions
revs
# Extract the user element
user_time <- list.select(revs,user,timestamp)

# Print user_time
user_time


# Load dplyr
library(dplyr)

# Pull out revision list
revs <- content(resp_json)$query$pages$`41916270`$revisions

# Extract user and timestamp
revs %>%
  bind_rows() %>%           
  select(user, timestamp)

# Stack to turn into a data frame
list.stack(user_time)



#Practice
# Load httr
library(httr)
library(rvest)
# The API url
base_url <- "https://en.wikipedia.org/w/api.php"

# Set query parameters
query_params <- list(action = "parse", 
                     #page = "Hadley Wickham",
                     #page = "Barack Obama",
                     format = "xml")

# Get data from API
resp <- GET(url = base_url, query = query_params)

# Parse response
resp_xml <- content(resp)
 

page_html <- read_html(xml_text(resp_xml))

# Extract infobox element
infobox_element <- html_node(page_html,css = ".infobox")

# Extract page name element from infobox
page_name <- html_node(infobox_element, css = ".fn")

# Extract page name as text
page_title <- html_text(page_name)


wiki_table <- html_table(infobox_element)
colnames(wiki_table) <- c("key", "value")
cleaned_table <- subset(wiki_table, !key == "")

# Create a dataframe for full name
name_df <- data.frame(key = "Full name", value = page_title)

# Combine name_df with cleaned_table
wiki_table2 <- rbind(name_df, cleaned_table)

# Print wiki_table
wiki_table2



#Turning the above into a function for reproducibilty
library(httr)
library(rvest)
library(xml2)

get_infobox <- function(title){
  base_url <- "https://en.wikipedia.org/w/api.php"
  
  # Change "Hadley Wickham" to title
  query_params <- list(action = "parse", 
                       page = title, 
                       format = "xml")
  
  resp <- GET(url = base_url, query = query_params)
  resp_xml <- content(resp)
  
  page_html <- read_html(xml_text(resp_xml))
  infobox_element <- html_node(x = page_html, css =".infobox")
  page_name <- html_node(x = infobox_element, css = ".fn")
  page_title <- html_text(page_name)
  
  wiki_table <- html_table(infobox_element)
  colnames(wiki_table) <- c("key", "value")
  cleaned_table <- subset(wiki_table, !wiki_table$key == "")
  name_df <- data.frame(key = "Full name", value = page_title)
  wiki_table <- rbind(name_df, cleaned_table)
  
  wiki_table
}

# Test get_infobox with "Hadley Wickham"
get_infobox(title = "Hadley Wickham")

# Try get_infobox with "Ross Ihaka"
get_infobox(title = "Chris Baywood Ibe")


webpage <- read_html("https://www.nytimes.com/interactive/2017/06/23/opinion/trumps-lies.html")
webpage



results <- webpage %>% html_nodes(".short-desc")

results


first_result <- results[1]

first_result %>% html_nodes("strong")

date <- first_result %>% html_nodes("strong") %>% html_text(trim = TRUE)



library(stringr)

str_c(date, ', 2017')

#> [1] "Jan. 21, 2017" 