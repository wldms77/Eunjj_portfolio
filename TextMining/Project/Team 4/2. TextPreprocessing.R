####################################################################################################
### Text Mining Team Project                                                                     ###
### Project Title: Analysis of Public Needs through Text Mining based on National Petition Board ###
### Project Goal: Extract Key Words of Each Department and LDA Topic Modelling                   ###
###               Find Inherent Needs of Public and Make a Suggestion to Legislator              ###
### Project Members: 21900471 Jeongin Yook (https://github.com/jeongin777)                       ###
###                  22000294 Jemin   Park (https://github.com/tori4913)                         ###
###                  22100809 Eunji   Hwang(https://github.com/wldms77)                          ###
####################################################################################################

########################################################
# <2. Text Pre-processing & Keywords Extractions > 
########################################################


library(dplyr)
library(stringr)
library(tidyr)
library(KoNLP)

useSejongDic()  # Load Sejong dictionary

# Add stopwords
stopwords <- c(
  "그리고", "하지만", "또한", "정말", 
  "하는", "위해", "하여", "있다", 
  "없다",'하게', "으로", "에서", 
  "하고", "대하", "위하", "아니",
  "따르", "에게", "부터", "까지", 
  "관하", "인하", "못하",'청원',
  '경우','생각'
)

stopwords <- unique(c(stopwords, intToUtf8(44032:55203, multiple = TRUE), letters, LETTERS, as.character(0:9)))

# Stemming function
stem_text <- function(text) {
  pos_results <- SimplePos22(text)  # Morphological analysis
  matches <- str_match_all(pos_results, "\\b([가-힣]+)/[NPVJ]")  # Extract nouns (N), verbs (V), adjectives (J)
  
  # Corrected line: Extract matched words (second column from match)
  stems <- unlist(lapply(matches, function(x) x[, 2]))  # Extract the second column (words)
  
  # Apply stemming rules by removing common suffixes
  stems <- gsub("한$|는$|을$|를$|에$|의$|하는$|같은$|입니$|위하여$", "", stems)  # Remove suffixes
  
  return(stems)  # Return the processed words
}


# Function to extract nouns only
extract_nouns <- function(words) {
  pos_results <- SimplePos22(paste(words, collapse = " "))  # Perform morphological analysis on word list
  matches <- str_match_all(pos_results, "\\b([가-힣]+)/N")  # Extract nouns (N) only
  
  # Corrected: Extract nouns from the second column of matches
  nouns <- unlist(lapply(matches, function(x) x[, 2]))  # Use function to extract the second column
  
  return(nouns)  # Return extracted nouns
}


# Keyword analysis function
analyze_keywords <- function(label, petition_data, stopwords) {
  # Filter data by label
  filtered_data <- petition_data %>%
    filter(label == !!label)
  
  # Function to extract keywords
  extract_keywords <- function(text) {
    clean_text <- str_replace_all(text, "[^가-힣\\s]", "") %>% str_squish()  # Remove special characters
    stems <- stem_text(clean_text)  # Extract stems
    nouns <- extract_nouns(stems)  # Extract nouns only
    keywords <- nouns[!nouns %in% stopwords]  # Remove stopwords
    keywords <- keywords[nchar(keywords) > 1]  # Remove single-character words
    return(keywords)
  }
  
  # Extract Title keywords
  title_keywords <- filtered_data %>%
    mutate(keywords = sapply(title, extract_keywords)) %>%
    unnest(keywords) %>%
    count(keywords, sort = TRUE)
  
  # Extract Text keywords
  text_keywords <- filtered_data %>%
    mutate(keywords = sapply(text, extract_keywords)) %>%
    unnest(keywords) %>%
    count(keywords, sort = TRUE)
  
  # Return results
  list(label = label, title_keywords = title_keywords, text_keywords = text_keywords)
}





