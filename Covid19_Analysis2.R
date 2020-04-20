
library(tidyverse)
library(lubridate)


csv_url <- "https://raw.githubusercontent.com/datasets/covid-19/master/data/time-series-19-covid-combined.csv"
destfile <- "Analysis2_global_cases_deaths2.csv"

# Download the file with download.file()
download.file(url = csv_url, destfile = destfile)

# Read it in with read.csv()
csv_data <- read.csv(destfile, stringsAsFactors = FALSE)

covid_data <- csv_data %>% 
  rename("Country"="Country.Region") %>% 
  filter(#Country %in% c("United Arab Emirates","Canada","United Kingdom","France","Italy","Nigeria","Spain","US","Germany","Dubai")
         (Province.State == '' | Country=="Canada")) %>% 
  select(1,2,6:8) %>% 
  arrange(Country,desc(Date))
  
covid_Analysis2_data <- covid_data %>% 
  mutate(Month = strftime(Date,"%b") ,
         Country = if_else(Country=="United Kingdom","UK",
                    if_else(Country=="United Arab Emirates","UAE",Country)))%>% 
  group_by(Month,Country) %>% 
  summarise(Max_confirmed = max(Confirmed),
            Max_death = max(Deaths)) %>% 
  arrange(desc(Month),desc(Max_death)) %>% 
  ungroup() %>% 
  top_n(10,Month) %>% 
  head(30) #%>% 
  #ungroup()

  #mutate(stdrd_value = Max_confirmed/max(Max_confirmed))

#View(covid_data)
View(covid_data2)
covid_data2 %>%
  ggplot(aes(x=Month,y=Max_death,fill = Country)) +
  geom_line(aes(col=Country),size = 1.2) +
  geom_point(aes(col=Country),size= 3)+
  labs(title = "Global Covid-19 cases", subtitle = "Total deaths")+
  geom_label(
           data=covid_data2 %>% filter(Month == 4), # Filter data first
          aes(label=paste0(Country,'-',round(Max_death,0))),
         nudge_x = 0.15, nudge_y = 0.25)+
   theme(legend.position = "none"
        # ,legend.key = element_rect(fill = "white", colour = "black")
        # ,legend.box.background = element_rect(color="red", size=2)
        # ,legend.text = element_text(size = 8, colour = "red")
         ) +
  guides(colour = guide_legend(override.aes = list(size = 10)),
         check_overlap = TRUE)
#)
 #
  

