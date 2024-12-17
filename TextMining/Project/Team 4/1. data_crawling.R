####################################################################################################
### Text Mining Team Project                                                                     ###
### Project Title: Analysis of Public Needs through Text Mining based on National Petition Board ###
### Project Goal: Extract Key Words of Each Department and LDA Topic Modelling                   ###
###               Find Inherent Needs of Public and Make a Suggestion to Legislator              ###
### Project Members: 21900471 Jeongin Yook (https://github.com/jeongin777)                       ###
###                  22000294 Jemin   Park (https://github.com/tori4913)                         ###
###                  22100809 Eunji   Hwang(https://github.com/wldms77)                          ###
####################################################################################################

########################################
# <1.Data Crawling> 
########################################


library(rvest)
library(dplyr)
library(tidyr)
library(stringr)

# Get URL (page number is replaced by %d)
# Caution! Web Crawling Date is Dec 1, 2024. Petition is keep updating on the website.  
base_url <- "https://www.cheongwon.go.kr/portal/petition/open/view?pageIndex=%d&searchType=1&slideYn=Y&searchBgnDe=2022-12-01&searchEndDe=2024-12-01&dselectw=7&searchPtnTtl=&searchPtnCn=&searchInstNm=&rlsStts=01%%2C02%%2C03&rlsSttsArr=01&rlsSttsArr=02&rlsSttsArr=03&type=card"

# Make an empty data frame 
petition_data <- data.frame(page_id = integer(), title = character(), text = character(), label = character(), date = character() , stringsAsFactors = FALSE)

# Repeat from page 1 to page 247 
for (i in 1:247) {
  # Set url of each page 
  url <- sprintf(base_url, i)
  
  # Get web page html data 
  web_page <- read_html(url)
  
  # Extract the title of the petition 
  petition_title <- web_page %>%
    html_nodes(".subject") %>%  
    html_attr("title") 
  
  # Extract the text of the petition 
  petition_text <- web_page %>%
    html_nodes(".text") %>% 
    html_text(trim = TRUE) 
  
  # Extract the category of the petition 
  petition_label <- web_page %>%
    html_nodes(".category") %>% 
    html_text(trim = TRUE) 
  
  # Extract the date of the petition 
  petition_date <- web_page %>% 
    html_nodes(".date") %>% 
    html_text(trim = TRUE) %>% 
    str_extract("\\d{4}.\\d{2}.\\d{2}") 
  
  # Add the result to the data frame
  page_data <- data.frame(page_id = rep(i, length(petition_title)),
                          title = petition_title,
                          text = petition_text,
                          label = petition_label,
                          date = petition_date,
                          stringsAsFactors = FALSE)
  
  # Combine data frames 
  petition_data <- bind_rows(petition_data, page_data)
}

# Check the data 
petition_data %>% head()
petition_data$page_id %>% length()
petition_data %>% is.na() %>% sum()
petition_data %>% summary()

# Save the data as RData file 
save(petition_data, file="petition_data.RData")

# Load the data
load("petition_data.RData")

# Recheck the data 
petition_data %>% head()

