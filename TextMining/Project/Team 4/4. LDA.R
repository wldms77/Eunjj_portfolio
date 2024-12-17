####################################################################################################
### Text Mining Team Project                                                                     ###
### Project Title: Analysis of Public Needs through Text Mining based on National Petition Board ###
### Project Goal: Extract Key Words of Each Department and LDA Topic Modelling                   ###
###               Find Inherent Needs of Public and Make a Suggestion to Legislator              ###
### Project Members: 21900471 Jeongin Yook (https://github.com/jeongin777)                       ###
###                  22000294 Jemin   Park (https://github.com/tori4913)                         ###
###                  22100809 Eunji   Hwang(https://github.com/wldms77)                          ###
####################################################################################################

##########################################
# < 4. Topic Modeling > 
##########################################

# Load the RData file containing the petition data
load("~/Downloads/petition_data.RData")

# Load necessary libraries for text mining, topic modeling, and Korean language processing
library(topicmodels)
library(ldatuning)
library(dplyr)
library(KoNLP)
library(stringr)
library(tm)

# 1. Define stopwords (common words that will be excluded from the analysis)
stopwords <- c("그리고", "하지만", "또한", "정말", "하는", "위해", "하여", "있다", "없다", "경우", "사람", "청원",
               "공개", "청원합니다", "관련", "생각", "문제", "상황", "국민", "본인", "대한민국", "이상", "해당", "제도",
               "필요", "사항", "처리", "답변", "국가", "아이들", "사형", "강화", "발생", "개선", "피해", "기준", "개정",
               "내용", "제조", "만원", "서울시", "근거", "반영", "학생", "제호", "결정", "이유", "이하", "들이", "하기", 
               "유현사거리","합니", "고가도로", "원당태리", "정책", letters, LETTERS, as.character(0:9))

# Add words ending with "니" to the stopwords list
stopwords <- c(stopwords, grep(".*니$", c("반대합니","바랍니" ,"필요합니","청원합니", "부탁드립니", "합니다", "합니다", "되어", "합시다", "요구합니", "요청합니"), value = TRUE))

# Add words ending with "니다" to the stopwords list
stopwords <- c(stopwords, grep(".*니다$", c("청원합니다", "부탁드립니다", "제출합니다", "검토합니다", "답변드립니다", "규제합니다"), value = TRUE))

# Ensure all characters are unique in the stopwords list and add Korean characters, letters, and numbers
stopwords <- unique(c(stopwords, 
                      intToUtf8(44032:55203, multiple = TRUE), 
                      letters, LETTERS, 
                      as.character(0:9)))

# 2. Function to extract nouns from text using Korean morphological analysis
extract_nouns <- function(text) {
  pos_results <- SimplePos22(text)  # Perform morphological analysis
  matches <- str_match_all(pos_results, "\\b([가-힣]+)/N")  # Extract only nouns (marked as 'N')
  nouns <- unlist(lapply(matches, function(x) x[, 2]))  # Return the extracted nouns
  return(nouns)
}

# 3. Preprocessing function to clean and process petition titles
preprocess_titles <- function(titles, stopwords) {
  titles %>%
    sapply(function(x) str_replace_all(x, "[^가-힣\\s]", "")) %>%  # Remove non-Korean characters
    str_squish() %>%                                               # Remove extra spaces
    sapply(extract_nouns) %>%                                      # Extract only nouns
    sapply(function(x) x[!x %in% stopwords]) %>%                   # Remove stopwords
    unlist()  # Flatten the list
}

# 4. Convert tokens into a Document-Term Matrix (DTM) for topic modeling
create_corpus <- function(tokens) {
  corpus <- Corpus(VectorSource(tokens))  # Create a corpus from the tokens
  dtm <- DocumentTermMatrix(corpus)  # Create a Document-Term Matrix (DTM)
  return(dtm)
}

# Main Analysis

# Preprocess the petition titles by extracting nouns and removing stopwords
preprocessed_tokens <- preprocess_titles(petition_data$title, stopwords)

# Create a Document-Term Matrix (DTM) from the preprocessed tokens
dtm <- create_corpus(preprocessed_tokens)

# 5. Use ldatuning to find the best number of topics based on various metrics
result <- FindTopicsNumber(
  dtm,
  topics = seq(2, 10, by = 1),  # Test for number of topics from 2 to 10
  metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
  method = "Gibbs",             # Use Gibbs sampling for LDA training
  control = list(seed = 1234),  # Set a fixed random seed
  mc.cores = 2L,                # Use 2 cores for parallel processing
  verbose = TRUE                # Show progress updates
)

# 6. Visualize the results of the topic modeling (optimal number of topics)
FindTopicsNumber_plot(result)


# 7. Perform Latent Dirichlet Allocation (LDA) with a specified number of topics (k)
perform_lda <- function(dtm, k = 5) {
  lda_model <- LDA(dtm, k = k, control = list(seed = 1234, alpha = 0.5))  # Fit the LDA model
  topics <- terms(lda_model, 8)  # Extract top 8 terms for each topic
  list(model = lda_model, topics = topics)  # Return both the model and the top terms
}

# Perform LDA to identify 5 topics
lda_results <- perform_lda(dtm, k = 5)

# Output the top 8 terms for each of the identified topics
print(lda_results$topics)

