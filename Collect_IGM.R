###########################
# File: Collect_IGM.R
# Description: 
#   (1) Collect (scrape) econ survey responses from IMG Chicago
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
  str_c(base_igm_url, .) %>%  # Note magrittr argument placeholder
  unique()

# Initialize an empty data frame to collect results
final <- data_frame()

# Loop through question links
for (i in survey_links) {
  
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
  
  # Count the questions
  nquestions <- doc %>%
    html_nodes(xpath= "//h3[@class='surveyQuestion']") %>%
    length()
  
  # Build vector of question ids
  qids <- str_c("Question ", LETTERS[1:nquestions], ":")
  
  if (nquestions == 1) {
    
    # Get the question if only one
    questions <- doc %>% 
      html_nodes(xpath = "//h3[@class='surveyQuestion']") %>%
      html_text() %>% 
      str_trim()
    
    if (length(questions) == 0) {
      
      # If empty use alternative xpath
      questions <- doc %>% 
        html_nodes(xpath = "//h3[@class='surveyQuestion']/following-sibling::p") %>%
        html_text() %>% 
        str_trim()      
    }
    
  }
  
  if (nquestions > 1) {
    
    # Get the questions and overwrite questions object if multiple Qs
    questions <- doc %>%
      html_nodes(xpath = "//h3[@class='surveyQuestion']") %>%
      html_text() %>% 
      str_replace_all(pattern = "Question\\s\\w+:", replacement = "") %>%
      str_trim()
    
  }
  
  # Get the table node
  tables <- doc %>%
    html_nodes("table") %>%
    html_table(fill = TRUE, trim = TRUE) %>%
    setNames(nm = qids)
  
  # Collapse responses data frames into single df
  responses <- tables %>% 
    plyr::ldply() %>% # Use plyr rbind until dplyr supports adding id
    filter(!is.na(`Bio/Vote History`)) %>%
    select(-`Bio/Vote History`) # drop column
  
  # Creat question data frame
  qdf <- data_frame(.id = qids, Date = date, Category = category, 
                    Question = questions)
  
  # Merge question metadata to responses
  combined <- left_join(qdf, responses, by = ".id")

  # Add results to empty data frame
  final <- rbind_list(final, combined)
  
  # Be polite to server
  Sys.sleep(time = 2)
  
}

# Output data to csv file
write.csv(final, "img_survey_responses.csv")
