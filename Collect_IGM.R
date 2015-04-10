###########################
# File: Collect_IGM.R
# Description: 
#   (1) Harvest (scrape) survey responses from IMG Chicago
#   (2) Loop through list of survey links and parse html using xpath selectors
#   (3) Collect the results into a data_frame and output results to a csv file
# Date: 4/09/2015
# Author: Jake Russ
# Notes: 
# To do:
###########################

library("rvest")    # devtools::install_github("hadley/rvest")
library("magrittr") # Adds pipe operator
library("stringr")  # Set of string manipulation tools
library("dplyr")

# Working directory 
dir <- getwd()

# Web adress to scrape
base_igm_url  <- "http://www.igmchicago.org"
igm_links_url <-  paste0(base_igm_url, "/igm-economic-experts-panel")

# Use rvest's html function to read in html source document (encoding is optional)
doc <- html(x = igm_links_url, encoding = "UTF-8")

# Get the survey links
survey_links <- doc %>%
  html_nodes(xpath = "//h3[@class='surveyQuestion']/a[@href][1]") %>%
  html_attr(name = "href") %>%
  str_c(base_igm_url, .) # Note magrittr argument placeholder

# Initialize an empty data frame to collect results
final <- data_frame()

# Loop through question links
for (i in survey_links) {
  
  i <- survey_links[2]
  
  doc <- html(i, encoding = "UTF-8")
  
  # Get the date
  date <- doc %>%
    html_nodes(xpath = "//h6") %>%
    html_text() %>%
    str_replace(pattern = "\\d+:\\d+[amp]+", replacement = "") %>%
    str_trim()

  # Get the category
  category <- doc %>%
    html_nodes(xpath = "//h2") %>%
    html_text() %>% 
    str_trim()
  
  # Get the question
  question <- doc %>%
    html_nodes(xpath = "//h3[@class='surveyQuestion']") %>% #/following-sibling::p") %>%
    html_text() %>% 
    str_replace_all(pattern = "Question\\s\\w+:", replacement = "") %>%
    str_trim()
  
  tables <- doc %>%
    html_nodes("table") %>%
    html_table(fill = TRUE, trim = TRUE)
  
  headers <- 

  responses <- tables %>% 
    rbind_all() %>%
    filter(Participant != "") %>%
    select(-`Bio/Vote History`) # drop column
  
  # Add question and date to results
  responses %<>% mutate(Question = question, 
                        Date     = date, 
                        Category = category) %>%
                 select(Date, Category, Participant:Question) # reorder columns
  
  # Add results to empty data frame
  final <- rbind_list(final, responses)
  
  # Be polite to server
  Sys.sleep(time = 2)
  
}

# Output data to csv file
write.csv(final, "img_survey_responses.csv")
