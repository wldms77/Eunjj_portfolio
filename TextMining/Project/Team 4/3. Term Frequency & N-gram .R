####################################################################################################
### Text Mining Team Project                                                                     ###
### Project Title: Analysis of Public Needs through Text Mining based on National Petition Board ###
### Project Goal: Extract Key Words of Each Department and LDA Topic Modelling                   ###
###               Find Inherent Needs of Public and Make a Suggestion to Legislator              ###
### Project Members: 21900471 Jeongin Yook (https://github.com/jeongin777)                       ###
###                  22000294 Jemin   Park (https://github.com/tori4913)                         ###
###                  22100809 Eunji   Hwang(https://github.com/wldms77)                          ###
####################################################################################################


############################
# <3. N-Gram Analysis> 
############################

#Import Necessary Libraries 
library(dplyr)
library(tidyr)
library(KoNLP)
library(wordcloud2)
library(stringr)
library(rvest)
library(dplyr)
library(stringr)
library(igraph)
library(ggraph)
library(tidytext)


##Based on the Text Pre-processing Rcode, 
# Extract the Keywords of Title and Text of each Labels.


for (result in all_results) {
  cat("Label:", result$label, "\n")
  
  # Print top 10 Title keywords
  cat("Top Title Keywords:\n")
  print(result$title_keywords %>% slice_head(n = 10))
  
  # Print top 10 Text keywords
  cat("Top Text Keywords:\n")
  print(result$text_keywords %>% slice_head(n = 10))
  
  cat("\n\n")
}

#Based on the above list result, Convert into Dataframe 
# Create the dataframe of Keyword for each Labels 
create_keyword_df <- function(all_results) {
  # Initialize the list to store the results
  keyword_list <- list()
  
  for (result in all_results) {
    label <- result$label  # Label Information
    top_keywords <- result$text_keywords %>% slice_head(n = 10)  
    
    # Convert label-specific keywords to data frames
    keyword_df <- top_keywords %>%
      mutate(label = label) %>%
      select(label, keyword = keywords, frequency = n)
    
    # Add to List
    keyword_list[[label]] <- keyword_df
  }
  
  # Merge list into one data frame
  final_keyword_df <- bind_rows(keyword_list)
  return(final_keyword_df)
}


# Generating Keyword Data Frames
keyword_df <- create_keyword_df(all_results)

# Check the result 
print(keyword_df, n= Inf) 

# After Checking the result, our team noticed that some words need to be deleted 

# Thus, Defined again the Stopwords
stopwords <- c( "경우","사람", "공개","청원합니다","관련","경우","생각","문제","상황","국민","본인","대한민국","이상","해당","제도","필요","사항",
                "답변","국가","아이들","사형","강화","발생","개선","피해","기준","내용", "제조", "만원","서울시","근거","반영",
                "학생","제호","결정","이유","이하","들이","하기","정책","저희", "때문",letters, LETTERS, as.character(0:9))



# Add unique stopwords to the existing list
stopwords <- unique(c(stopwords, intToUtf8(44032:55203, multiple = TRUE), letters, LETTERS, as.character(0:9)))

# Filter Stopwords
cleaned_keyword_df <- keyword_df %>%
  filter(!keyword %in% stopwords)


#we extract top 3 keywords for each label
top_keywords_all <- cleaned_keyword_df %>%
  group_by(label) %>%
  arrange(desc(frequency)) %>%  # Sort in descending order based on frequency
  slice_head(n = 3) %>%  # Select Top 3
  ungroup()  # Ungroup

# Check the result
print(top_keywords_all,n = Inf)





########## Implement the N-gram Analysis #######

#Before implement N-gram, do pre-processing 
preprocess_text <- function(text) {
  # Clean the text by removing non-Korean characters and extra spaces
  clean_text <- str_replace_all(text, "[^가-힣\\s]", "") %>% str_squish()  
  nouns <- extractNoun(clean_text)   # Extract nouns from the cleaned text
  # Filter out stopwords and single-character nouns
  filtered_nouns <- nouns[!nouns %in% stopwords & nchar(nouns) > 1]  
  return(filtered_nouns)
}

analyze_ngram <- function(label, keyword, petition_data, n = 3) {
  # Filter petition data for the specified label
  label_data <- petition_data %>%
    filter(label == !!label) %>%
    select(text)
  
  
  label_data <- label_data %>%
    # Preprocess text and combine processed text into a single string per row
    mutate(
      processed_text = sapply(text, preprocess_text)  
    ) %>%
    unnest(processed_text) %>%
    group_by(text) %>%
    summarize(full_text = paste(processed_text, collapse = " "))  
  
  ## Generate N-grams, filter by keyword, and count occurrences
  ngram_data <- label_data %>%
    unnest_tokens(ngrams, full_text, token = "ngrams", n = n) %>%   # Create N-grams
    filter(str_detect(ngrams, keyword)) %>%             # Keep N-grams containing the keyword          
    count(ngrams, sort = TRUE)                         #Count N-gram occurrences            
  
  return(ngram_data)
}

# Select the top keyword for each label
top_keywords <- top_keywords_all %>%
  group_by(label) %>%
  slice(1) %>% # set slice(n) : n = 1,2,3 to extracts the first, second, and third rows on top_keywords_all for each labels keywords
  select(label, keyword)

# Perform N-gram analysis for each label and keyword pair
ngram_results <- top_keywords %>%
  rowwise() %>%
  mutate(ngrams = list(analyze_ngram(label, keyword, petition_data))) %>% # Analyze N-grams
  unnest(ngrams)

#Display the N-gram result
ngram_results %>% group_by(label) %>% slice(1:10) %>% print(n=100)

