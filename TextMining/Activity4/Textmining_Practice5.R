############################################
### Course: Text Mining                #####
### Subject: 2024-2                    #####
### Title: Practice 05                 #####
### TEAM: 04                           #####    
### Member: 21900471 Jeongin Yook      #####
###         22000294 Jemin   Park      #####
###         22100809 Eunji   Hwang     #####
############################################

#Import necessary libraries
library(officer)
library(dplyr)
library(magrittr)
library(stringr)
library(tm)
library(textstem)

# < Question 1 – Tweets > #
# (1-1, 5 pts) Import “clintontrump_tweets.csv” to a variable called ct.tweet in R. 
# With ct.tweet, remove “entities” and “extended_entities” columns, 
# and create a new column called “candidate” which returns HillaryClinton 
# if the value of column is HillaryClinton or DonaldTrump if not.

#my file root
ct.tweet <-
  read.csv("~/TextMining_R/TextMining R/Practice 5/clintontrump_tweets.csv")

#ct.tweet <- read.csv("clintontrump_tweets.csv") # read the data 
ct.tweet <- ct.tweet %>% select(-entities, -extended_entities) # remove "entities" and "extended_entities" colunms 
# create new column "candidate" with the value of "HillaryClinton" if the value of "handle" is "HillaryClinton" and "DonaldTrump" if not 
ct.tweet <- ct.tweet %>% mutate(candidate = ifelse(handle == "HillaryClinton", "HillaryClinton", "DonaldTrump")) 
head(ct.tweet)


# (1-2, 15 pts) For this assignment, we will mainly use the text column. 
# Conduct text pre-processing operations: 
# 1) remove URL, 
# 2) remove any expression that follows the '@' symbol, 
# 3) remove Hashtags, 
# 4) remove \n,
# 5) remove numbers.

ct.tweet$text %>% head(10) 
ct.tweet$text %<>% str_remove_all("https[:print:]+") %>% # remove any printable characters that follow "https"
  str_remove_all("@[:alnum:]+") %>% # remove any alphanumeric characters that follow "@" 
  str_remove_all("#[:alnum:]+") %>% # remove any alphanumeric characters that follow "#" 
  str_remove_all("\n") %>% # remove "\n" 
  str_remove_all("[:digit:]") # remove numbers 

# Check the result
ct.tweet$text %>% head(10) 


# (1-3, 10 pts) If you check the 95th row of ct.tweet, you may find that its “text” column contains Emoji. 
# Using only the regular expression detecting Emoji written below, 
# create a function called rm_emoji and use it to update the “text” column.

# Check 95th row 
ct.tweet[95,]$text 

# remove Emoji from the text column 
#ct.tweet[95,]$text %>% str_remove_all("[\U{1F600}-\U{1F64F}\U{1F300}-\U{1F5FF}\U{1F680}-\U{1F6FF}\U{2600}-\U{26FF}\U{2700}-\U{27BF}-\U{1F1FA}]") 

#ct.tweet$text

 
emoji_pattern <- "[\U{1F600}-\U{1F64F}\U{1F300}-\U{1F5FF}\U{1F680}-\U{1F6FF}\U{2600}-\U{26FF}\U{2700}-\U{27BF}-\U{1F1FA}]"

# Revise
# Defined the function to remove the emoji_pattern with str_remove_all()
rm_emoji <- function(text) {
  str_remove_all(text, emoji_pattern)  
}
# Remove the emoji by applying the rm_emoji function to the text column in the ct.tweet data frame.
ct.tweet <- ct.tweet %>%
  mutate(text = rm_emoji(text))

# check the clear result 
ct.tweet[95,]$text


# (1-4, 15 pts) From text-preprocessed column, “text”, create two vectors called hc.vec and dt.vec, 
# which includes only texts related to Hilary Clinton and Donald Trump, respectively. Here, do not include retweet. 
# Using hc.vec and dt.vec, create a Vcorpus variable called ct.tweet.corpus.

# Create the Vectors,
# Filters for tweets by Hillary Clinton that are not retweets
hc.vec <- ct.tweet %>%
  filter(handle == "HillaryClinton", is_retweet == "False") %>%
  pull(text) # Extracts the "text" column to create a vector of tweet content for Hillary Clinton

# Filters for tweets by Donald Trump that are not retweets
dt.vec <- ct.tweet %>%
  filter(handle == "realDonaldTrump", is_retweet == "False") %>% 
  pull(text)  # Extracts the "text" column to create a vector of tweet content for Donald Trump

# Combining both vectors into a single text vector
combined_text <- c(hc.vec, dt.vec)

# Creating a VCorpus (corpus) from the combined text vector
ct.tweet.corpus <- VCorpus(VectorSource(combined_text))

#Inspecting the contents of the created corpus
ct.tweet.corpus %>% inspect
ct.tweet.corpus[[1]] %>% inspect

#Summarizing the corpus information
summary(ct.tweet.corpus)


# (1-5, 15 pts) Using this ct.tweet.corpus, create a new cleansed corpus called ct.tweet.
# cleaned by running following operations:
# 1) convert all into lower cases, 
# 2) remove stopwords (iso), 
# 3) remove punctuations, 
# 4) lemmatize, and 
# 5) remove numbers.

# Define the text cleaning function
ct.tweet.cleaned <- ct.tweet.corpus %>% 
  tm_map(content_transformer(tolower)) %>%  # Convert into lower case           
  tm_map(removeWords,stopwords("en")) %>%
  tm_map(removePunctuation) %>%   # Remove punctuation               
  tm_map(content_transformer(lemmatize_strings)) %>%  # implement lemmitization process
  tm_map(removeNumbers)   # remove numbers                         

#tm_map(removeWords,c(stopwords("en"), "gmt", "character", "hour", "year", "the", "will")) %>%  # Remove stopwods                

#Summarizing the ct.tweet.cleaned information
summary(ct.tweet.cleaned)

# check whether the ct.tweet.corpus been cleaned.
inspect(ct.tweet.corpus[[1]])

# (1-6, 10 pts) Using ct.tweet.cleaned, create Term Document Matrix called ct.tweet.dtm 
# and Document Term Matrix called ct.tweet.tdm. 
# Is there any sparse representation issue? 
# Explain with your own language.

# Create Term Document Matrix as ct.tweet.tdm 
ct.tweet.tdm <- TermDocumentMatrix(ct.tweet.cleaned)

# Create Document Term Matrix as ct.tweet.dtm
ct.tweet.dtm <- DocumentTermMatrix(ct.tweet.cleaned)

#Check the result of dtm and tdm 
inspect(ct.tweet.dtm)
inspect(ct.tweet.tdm)



# Explanation:
# Our result show the sparse as 100% which means, This means that nearly all of the cells in the matrix are zero, 
# indicating that most terms do not appear in each document.
# This status can lead to some issues, first is High Sparsity which reflect that most terms are unique or very rare throughout the document.
# And, due to the high sparsity, TDMs need to store a lot of zero values, which requires a significant amount of memory. 
# Furthermore, dealing with sparsity TDMs can be computationally inefficient.
# In addition, The large number of unique terms as a dimension problem can dilute the importance of each term with sparsity.

## Solve the sparse representation issue ## 
# use removeSparseTerms(), 
# If we set the sparsity criterion to 0.95, 
# It means that Remove words that do not appear in more than 95% of documents. 
# All words that appear in less than 5% of all documents are removed, leaving only words that appear relatively frequently in documents.
ct.tweet.tdm <- removeSparseTerms(ct.tweet.tdm, 0.95) 
inspect(ct.tweet.tdm) # check the sparsity again, it seem that decreased as 92%


# (1-7, 20 pts) What are the 10 top representative words?
# Do you think the results make sense? You must explain the logic of your operation and your opinion about it.
ct.tweet.dtm %>%  as.matrix %>%colSums() %>% sort(decreasing = T) %>% head(10)


# Sum term frequencies across all documents in the TDM
term_freq <- rowSums(as.matrix(ct.tweet.tdm))

# Sort terms by frequency in descending order 
top_terms <- sort(term_freq, decreasing = TRUE)

# Extract the top 10 terms
top_10_terms <- head(top_terms, 10)

# Display the top 10 terms and their frequencies
top_10_terms

#Logic explanation :
# This is based on the number of appearances of words that occurred in the Twitter data.
# Use the as.matrix() function to transform a term-frequency matrix (TDM) into a matrix, 
# rowSums() to obtain the total frequency of each word.
# Sort (decreasing = TRUE) to sort the words in the most frequent order. 
# This makes it easy to find the most used words.
# Use head (10) to select the top 10 words. 

#Personal opinion:
# The fact that positive words such as "thank," "great," and "make" are on the top is 
# likely to have something to do with users expressing positive feelings 
# about political situations, political messages, and campaigns.

# The top 1o frequency terms are trump, hillary, thank, great,  make, get, donald
# But, we need to think about the result such as the word trump and donald is a name of one words,
# so, this result need more advanced approach. 


# (1-8, 10 pts) Are Clinton’s tweet and Trump’s tweet similar? Choose two similarity measures, measure them and explain.

#1) extract eact text of Hillary and Trump 

# Filter tweets from Hillary Clinton and select only non-retweets
hc_df <- ct.tweet %>%
  filter(handle == "HillaryClinton", is_retweet == "False") %>%
  select(text) %>%
  mutate(author = "Hillary")# Add a new column indicating the author as "Hillary"

# Filter tweets from Donald Trump and select only non-retweets
dt_df <- ct.tweet %>%
  filter(handle == "realDonaldTrump", is_retweet == "False") %>%
  select(text) %>%
  mutate(author = "Trump") # Add a new column indicating the author as "Trump"

#combined 
combined_df <- rbind(hc_df, dt_df)
combined_df %>% head

#Import libraries
library(tm)
library(proxy)
library(dplyr)

# 2) convert into Corpus, and TDM

# Create the Hillary TermDocumentMatrix 
hc_corpus <- Corpus(VectorSource(hc_df$text)) # convert into Corpus 
hc_tdm <- TermDocumentMatrix(hc_corpus) # create TermDocumentMatrix
hc_sparse_matrix <- as.matrix(hc_tdm) # convert into matrix 
hc_tdm <- removeSparseTerms(hc_tdm, 0.99) # improve the sparsity as 0.95
hc_tdm %>% inspect

hc_sparse_matrix <- as.matrix(hc_tdm) # convert into matrix 

# 3) Similarity Calculation 

# Calculate the cosine distance 
hc_cosine_similarity <- 1 - dist(hc_sparse_matrix, method = "cosine")
# the reason why 1 minus is that to get the result of Cosine Similarity,
# 1 shows perfect similarity and 0 shows complete dissimilarity.

# Convert into dataframe hc_cosine_similarity
hc_similarity_df <- as.data.frame(as.matrix(hc_cosine_similarity))

#display the result 
hc_similarity_df


# 3) Display the Similarity 


# create the stopwords list 
stopwords <- c("and", "the", "is", "in", "to", "for", "with", "a", "of", "that", "this", "it", "you", 
               "who", "has", "have", "not", "are", "was", "be", "at", "on", "as", "which", "by", 
               "all", "just", "its", "he", "she", "they", "but", "so", "if", "than", "about", 
               "my", "his", "her", "their", "our", "your", "me", "us", "him", "them", 
               "donald", "trump", "hillary", "will", "thank", "you", "too", "many", "each", "other",
               "make", "sure", "we're", "going")


# Remove the terminology and specific words from the similarity matrix
filtered_similarity_df <- hc_similarity_df[!(rownames(hc_similarity_df) %in% stopwords), 
                                           !(colnames(hc_similarity_df) %in% stopwords)]

# Transform filtered matrices into vectors
filtered_similarity_matrix <- as.matrix(filtered_similarity_df)
filtered_similarity_vector <- as.vector(filtered_similarity_matrix)

# remove the NA value 
filtered_similarity_vector <- filtered_similarity_vector[!is.na(filtered_similarity_vector)]

# Sort with the index of similarity values
sorted_filtered_indices <- order(filtered_similarity_vector, decreasing = TRUE)

# Extract Top 10 Similarities
top_10_filtered_indices <- sorted_filtered_indices[1:10]

# Transform an index into rows and columns of a matrix
row_col_filtered_indices <- arrayInd(top_10_filtered_indices, dim(filtered_similarity_matrix))

# Display the result of top 10 similarities as dataframe
hill_top_cos_filtered_results <- data.frame(
  Row = rownames(filtered_similarity_matrix)[row_col_filtered_indices[, 1]],
  Column = colnames(filtered_similarity_matrix)[row_col_filtered_indices[, 2]],
  Similarity = filtered_similarity_vector[top_10_filtered_indices]
)

# Display the result
print(hill_top_cos_filtered_results)

#Explnation : We see that Hillary Clinton's cosine approach frequently features "watch-live" , and ""child-care", and "women-first"
#This confirms that show the  social and political agenda related to this words
# And, "united-president" could emphasizes United leadership, national unity

######################################################################################################
# Use euclidean distance 
hc_euclidean_similarity <- dist(hc_sparse_matrix, method = "euclidean")

# Convert into dataframe hc_euclidean_similarity
hc_similarity_df <- as.data.frame(as.matrix(hc_euclidean_similarity))

#check the dataframe
hc_similarity_df



# Defining a list of words to exclude and non-verbal terms
stopwords <- c("and", "the", "is", "in", "to", "for", "with", "a", "of", "that", "this", "it", "you", 
               "who", "has", "have", "not", "are", "was", "be", "at", "on", "as", "which", "by", 
               "all", "just", "its", "he", "she", "they", "but", "so", "if", "than", "about", 
               "my", "his", "her", "their", "our", "your", "me", "us", "him", "them", 
               "donald", "trump", "-hillary","hillary", "will",  "-", "—",
               "thank", "you", "too", "many", "each", "other",
               "make", "sure", "we're", "going")

# Remove the terminology and specific words from the similarity matrix
#filtered_similarity_df <- hc_similarity_df[!(rownames(hc_similarity_df) %in% stopwords), 
                                          # !(colnames(hc_similarity_df) %in% stopwords)]

# Remove the terminology and specific words from the similarity matrix
filtered_similarity_df <- hc_similarity_df[!sapply(rownames(hc_similarity_df), 
                                                  function(x) any(grepl(paste(stopwords, collapse = "|"), x))),
                                          !sapply(colnames(hc_similarity_df), 
                                                  function(x) any(grepl(paste(stopwords, collapse = "|"), x)))]

# Transform filtered matrices into vectors

filtered_similarity_matrix <- as.matrix(filtered_similarity_df)
filtered_similarity_vector <- as.vector(filtered_similarity_matrix)

# Remove NA Value
filtered_similarity_vector <- filtered_similarity_vector[!is.na(filtered_similarity_vector)]

# Sort with the index of similarity values
sorted_filtered_indices <- order(filtered_similarity_vector, decreasing = TRUE)

# Extract Top 10 Similarities
top_10_filtered_indices <- sorted_filtered_indices[1:10]

# Transform an index into rows and columns of a matrix
row_col_filtered_indices <- arrayInd(top_10_filtered_indices, dim(filtered_similarity_matrix))

# display the result as dataframe
hill_top_euc_filtered_results <- data.frame(
  Row = rownames(filtered_similarity_matrix)[row_col_filtered_indices[, 1]],
  Column = colnames(filtered_similarity_matrix)[row_col_filtered_indices[, 2]],
  Similarity = filtered_similarity_vector[top_10_filtered_indices]
)

# display the result
print(hill_top_euc_filtered_results)

cbind(hill_top_cos_filtered_results, hill_top_euc_filtered_results)

#Explanation_rev : the numbers increased, it means the silmilarity is low. Thus, here word people seems to be used in a various context 
# the words like more, president, from, would, need are not usually used with people

#Explanation : Hillary Clinton's cosine approach comes with a lot of make in common and present.
# As a president, I can see how confident I can achieve something.


#################--Trump--#############################

# Create a text corpus from the tweets of Donald Trump
dt_corpus <- Corpus(VectorSource(dt_df$text))
# Create a Term-Document Matrix (TDM) from the corpus
dt_tdm <- TermDocumentMatrix(dt_corpus) # Create a Term-Document Matrix (TDM) from the corpus
dt_sparse_matrix <- as.matrix(dt_tdm) # Convert into matrix
dt_tdm <- removeSparseTerms(dt_tdm, 0.99) # Remove sparse terms from the Term-Document Matrix (keeping terms that appear in at least 10% of the documents)
dt_sparse_matrix <- as.matrix(dt_tdm)

# Calculate the cosine similarity matrix (1 - cosine distance) from the sparse matrix
dt_cosine_similarity <- 1 - dist(dt_sparse_matrix, method = "cosine")

# Convert the cosine similarity matrix to a data frame for easier analysis
dt_similarity_df <- as.data.frame(as.matrix(dt_cosine_similarity))
dt_similarity_df



# Defining a list of words to exclude and non-verbal terms
stopwords <- c("and", "the", "is", "in", "to", "for", "with", "a", "of", "that", "this", "it", "you", 
               "who", "has", "have", "not", "are", "was", "be", "at", "on", "as", "which", "by", 
               "all", "just", "its", "he", "she", "they", "but", "so", "if", "than", "about", 
               "my", "his", "her", "their", "our", "your", "me", "us", "him", "them", 
               "donald", "trump", "hillary", "will", "thank", "you")

# Remove the terminology and specific words from the similarity matrix
filtered_similarity_df <- dt_similarity_df[!(rownames(dt_similarity_df) %in% stopwords), 
                                           !(colnames(dt_similarity_df) %in% stopwords)]

# Transform filtered matrices into vectors
filtered_similarity_matrix <- as.matrix(filtered_similarity_df)
filtered_similarity_vector <- as.vector(filtered_similarity_matrix)

# Remove the NA value
filtered_similarity_vector <- filtered_similarity_vector[!is.na(filtered_similarity_vector)]

# Sort with the index of similarity values
sorted_filtered_indices <- order(filtered_similarity_vector, decreasing = TRUE)

# Extract Top 10 Similarities
top_10_filtered_indices <- sorted_filtered_indices[1:10]

# Transform an index into rows and columns of a matrix
row_col_filtered_indices <- arrayInd(top_10_filtered_indices, dim(filtered_similarity_matrix))

# Display the result 
Tum_top_cos_filtered_results <- data.frame(
  Row = rownames(filtered_similarity_matrix)[row_col_filtered_indices[, 1]],
  Column = colnames(filtered_similarity_matrix)[row_col_filtered_indices[, 2]],
  Similarity = filtered_similarity_vector[top_10_filtered_indices]
)

#Display the result 
print(Tum_top_cos_filtered_results)

#Explanation_rev : we can see the Trump's campaign slogan of 'Make America great again!', 
# so related words show the high similarity. and the words of sandres-burnile 
# which is can be inferred that political confrontation is a name often mentioned when making related remarks.
#alos, 'cruz-ted'likely referring to political confrontation.

#Explanation : Donald Trump's Cosine approach shows that "america" and "make", "great" the highest similarity. 
#It seems because of Donald Trump's campaign slogan was "Make America Great Again".



# Calculate the euclidean distance 
dt_eucli_similarity <- dist(dt_sparse_matrix, method = "euclidean")

dt_similarity_df <- as.data.frame(as.matrix(dt_eucli_similarity))
dt_similarity_df

# Defining a list of words to exclude and non-verbal terms
stopwords <- c("and", "the", "is", "in", "to", "for", "with", "a", "of", "that", "this", "it", "you", 
               "who", "has", "have", "not", "are", "was", "be", "at", "on", "as", "which", "by", 
               "all", "just", "its", "he", "she", "they", "but", "so", "if", "than", "about", 
               "my", "his", "her", "their", "our", "your", "me", "us", "him", "them", 
               "donald", "trump", "hillary", "will", "thank", "you", "&amp;")

# Remove the terminology and specific words from the similarity matrix
filtered_similarity_df <- dt_similarity_df[!(rownames(dt_similarity_df) %in% stopwords), 
                                           !(colnames(dt_similarity_df) %in% stopwords)]

# Transform filtered matrices into vectors
filtered_similarity_matrix <- as.matrix(filtered_similarity_df)
filtered_similarity_vector <- as.vector(filtered_similarity_matrix)

# Remove the NA value
filtered_similarity_vector <- filtered_similarity_vector[!is.na(filtered_similarity_vector)]

# Sort with the index of similarity values
sorted_filtered_indices <- order(filtered_similarity_vector, decreasing = TRUE)

# Extract Top 10 Similarities
top_10_filtered_indices <- sorted_filtered_indices[1:10]

# Transform an index into rows and columns of a matrix
row_col_filtered_indices <- arrayInd(top_10_filtered_indices, dim(filtered_similarity_matrix))

# Display the result 
Tum_top_euc_filtered_results <- data.frame(
  Row = rownames(filtered_similarity_matrix)[row_col_filtered_indices[, 1]],
  Column = colnames(filtered_similarity_matrix)[row_col_filtered_indices[, 2]],
  Similarity = filtered_similarity_vector[top_10_filtered_indices]
)

#Display the result 
print(Tum_top_euc_filtered_results)

##Explanation : 
# It can be interpreted that the higher the Euclidean distance value, the lower the similarity.
#So, the word great is used to convey or emphasize a positive message, which comes with it
#Words combined with "crooked" (term crooked used to When criticizing the Democratic Party) 
# and "cruz" are likely used by Trump to highlight his political position or criticism of his rivals
#Therefore, the distance between the two is far, suggesting that they were not used together.



cbind(hill_top_cos_filtered_results, Tum_top_cos_filtered_results)
#Conclusion: 
# In the case of Trump, word pairs such as again! - america are highly similar, indicating a strong campaign message corresponding to Trump's slogan.
# On the other hand, hillary, pairs of words such as care-child relate to social issues addressed by Hillary, 
# which are highly similar in remarks emphasizing social responsibility.
# Thus, Hillary tends to comment on issues related to social responsibility, individual rights, and children, 
# and Trump may as well make the difference highlighting American reconstruction and political confrontation.




######################### < Question 2 – BBC > ###############################

#(2-1, 5 pts) Import all .txt files from “bbc_text_set” folder to a variable called text.set, 
# which is a corpus object of tm package. text.set should contain 417 elements.

library(tm)

dir_path <- "~/Desktop/Textmining_Practice_team4/Textmining_Practice5_team4/bbc_text_set"

#my file  
dir_path <- "~/TextMining_R/TextMining R/Practice 5/bbc_text_set"

# Assign the path of text files.

text.set <- Corpus(DirSource(directory = dir_path, encoding = "UTF-8"), 
                   readerControl = list(language = "en"))

# Use the DirSource and Corpus features to create a corpus object and then pass it to test.set.

text.set %>% length
# we can check that length of test.set is 417

# (2-2, 10 pts) All .txt files included in “bbc_text_set” are news articles related to a specific topic. 
# To figure out the topic, we can simply start by analyzing the title of news article.
# In our sample, the first line of article is the news title. 
# Create a data.frame called text.title as shown below. Here id indicates the index number of .txt file. 
# From this, write down your own explanation on the topic of text.set.

library(tibble)
# import tibble library to make dataframe

text.set[[3]]$content %>% str_extract('[:print:]*\\n{2}') %>% str_trim()
# The pattern of topic in an object is \n You can see that it ends with two.

topic <-vector()
# Create an empty vector and pass it to the top variable.

for(i in 1:length(text.set)){
  topic[i]<-text.set[[i]]$content %>% str_extract('[:print:]*\\n{2}') %>% str_trim()
} 
# Use the iteration to obtain the Titles of all contents of text.set.

text.title<-data.frame(
  id=1:length(text.set),
  title=topic
)
# Create a data frame with the values obtained above.

text.title <- column_to_rownames(text.title, var = "id") 
# Resets the column name to make the data frame equal to the expected result.
text.title %>% head


# (2-3, 10 pts) Create a corpus object called text.set.clean by manipulating text.set. 
# As you can noticed from the name of variable, this should be a pre-processed variable, 
# which can help us to analyze a systematic text analysis of individual articles. 
# Explain your text pre-processing steps. Expected results are shown below (stopwords from nltk package were used).

library(textstem)
# import library we need 
iso_stopwords <- stopwords::stopwords("en", source = "nltk") 
# bring iso-stopwords as a stopwords with nltk option
text.set[[1]]$content %>% str_remove('[:print:]+\\n{2}') 
# remove title character

text.set.clean <- text.set %>%
  tm_map(content_transformer(function(x) str_remove(x, '[:print:]+\\n{2}'))) %>%  # Remove double newline characters
  tm_map(content_transformer(function(x) str_remove_all(x, "^\\s*$"))) %>%  # remove empty strings
  tm_map(content_transformer(lemmatize_strings)) %>%  # Lemmatize words (reduce to base form)
  tm_map(content_transformer(tolower)) %>%  # Convert text to lowercase
  tm_map(content_transformer(removeWords), iso_stopwords) %>%  # Remove stopwords (common words like "the", "is")
  tm_map(removePunctuation) %>%  # Remove punctuation (e.g., commas, periods)
  tm_map(removeNumbers) %>%  # Remove numbers from the text
  tm_map(stripWhitespace)  # Remove extra whitespace


text.set.clean[[1]]$content

# (2-4, 10 pts) Find representative word of each article. To answer this question, create a new data.frame called rep.df, 
# which contains columns of “id”, “representative_term”, and “value” (one that you measure for answering this question). 
# Explain your operation. Do you think what you have found from rep.df matches to the title of articles?

# Create Document-Term Matrix (DTM)
# This creates a document-term matrix (DTM) which represents the frequency of terms (words) in the corpus.
# Each row corresponds to a document, and each column corresponds to a term in the corpus.
dtm <- DocumentTermMatrix(text.set)

# Apply TF-IDF weighting to the DTM
# TF-IDF (Term Frequency-Inverse Document Frequency) is a weighting scheme used to evaluate the importance of a word in a document.
# It increases the weight of terms that appear frequently in a document, but decreases the weight of terms that appear frequently across all documents.
tfidf <- weightTfIdf(dtm)

# Find the word with the highest TF-IDF value for each document
# For each document (row), this finds the term with the highest TF-IDF score, which is considered the most "important" or "representative" word.
representative_words <- apply(as.matrix(tfidf), 1, function(x) {
  term <- names(x)[which.max(x)]  # Find the word with the maximum TF-IDF score in the document
  score <- max(x)                 # Get the TF-IDF score of that word
  list(term = term, score = score) # Return the word and its TF-IDF score as a list
})
# You can proceed with tfidf to see which words appear particularly frequently in each of the arts as a percentage.
# This allows you to obtain a representative word.

# Convert the list of representative words to a data frame
# This creates a data frame where each row represents a document and its most representative word and its TF-IDF score.
representative_words_df <- do.call(rbind, lapply(representative_words, as.data.frame))

# Set column names for the data frame
# The data frame will have columns "representative_word" for the word, and "tfidf_score" for the TF-IDF value.
colnames(representative_words_df) <- c("representative_word", "tfidf_score")
representative_words_df %>% head

# Display the data frame of representative words and their TF-IDF scores
representative_words_df %>% cbind(topic)

# Using tfidf, we found a representative word of each object. 
# We compare it with topic and check it.

#  You can see that the representative word comes out as words related to the topics in the topic. 
# Through this, we can see that in each article, one of the keywords in Title has features that often appear in the text.
# The same words don't always appear in the title. For example, if the tfidf_score is low,
# the representative word might be different from the word in the title.

# (2-5, 15 pts) To group similar articles, conduct hierarchical clustering analysis, and store the clustering result to text.set.cluster. 
# Explain your operation and visualize your clustering result.

# Create Document-Term Matrix (DTM)
dtm <- DocumentTermMatrix(text.set.clean)
# The DTM is a matrix representing the relationship between documents and words.
# It provides information on how often each word appears in each document.

dtm_matrix <- as.matrix(dtm)
# The DTM is stored as a Sparse Matrix by default. We convert it to a **regular matrix**.
# After conversion, it becomes a format that can be used to compute similarity between documents.

# Compute Euclidean distance
distance_matrix <- dist(dtm_matrix, method = "euclidean")
# Euclidean distance calculates the **distance** (i.e., similarity) between each pair of documents.
# When two documents are similar, the distance is small, and when they are dissimilar, the distance is large.
# `method = "euclidean"` specifies that the Euclidean distance metric is used.

# Perform hierarchical clustering
cluster <- hclust(distance_matrix, method = "ward.D2")
# Hierarchical clustering is performed.
# The `hclust()` function uses the distance matrix obtained from `dist()` to perform hierarchical clustering.
# `method = "ward.D2"` is used for the clustering method (Ward’s method)

# Load the factoextra package
library(factoextra)

# Visualize the dendrogram (display the hierarchical clustering result as a tree)
cluster %>%
  fviz_dend(k=5,                    # Divide the dendrogram into 5 clusters
            show_labels = FALSE,    # Do not display labels for documents (for a cleaner view)
            rect = TRUE,            # Display rectangles around each cluster
            color_labels_by_k = TRUE) # Color the labels according to their cluster

# Visualization 
# This is the visualization result of clustering 417 documents.
# Grouping a large number into clustering makes it difficult to identify the indexes of grouped documents. 
# The cluster at the bottom shows that the sentences are made up of the most similar characters.
# The higher up, the lower the similarity and the greater the intra-cluster.


# Convert the distance matrix to a regular matrix
distance_matrix <- as.matrix(distance_matrix)
# `distance_matrix` is stored as a `dist` object, so we convert it to a **matrix** to allow indexing and manipulation.

# Find the two documents with the smallest Euclidean distance
min_distance <- min(distance_matrix[distance_matrix > 0])  
# Find the smallest value in the `distance_matrix` that is greater than 0.
# `0` represents the distance from a document to itself, so we exclude it and focus on the actual distances between documents.

# Extract the indices of the documents corresponding to the minimum distance
closest_docs <- which(distance_matrix == min_distance, arr.ind = TRUE)[1, ]
# `which()` returns the index positions that satisfy the given condition.
# `distance_matrix == min_distance` finds the indices of the two documents with the smallest distance.
# `arr.ind = TRUE` returns the result in matrix index format. We select only the first position `[1, ]`.

# Indices of the two closest documents
doc1_index <- closest_docs[1]
doc2_index <- closest_docs[2]
# Store the indices of the two closest documents in `doc1_index` and `doc2_index`.

# Print the contents of the two documents
text.set.clean[[doc1_index]]$content
text.set.clean[[doc2_index]]$content
# Extract and print the actual content of the two closest documents based on their indices.
# Find index of content to retrieve the specific document content and compare the two closest documents.

# # After checking the two closest sentences,
# you can see that the two sentences use very similar words and have overlapping arts.











### title version ### 
# This time, we're going to do something similar, 
# and we're going to cluster through Titles in articles and we're going to look at similar sentences.

# Create Document-Term Matrix (DTM)
dtm <- DocumentTermMatrix(text_corpus)
# The DTM is a matrix representing the relationship between documents and words.
# It provides information on how often each word appears in each document.

# Convert the DTM to a regular matrix
dtm_matrix <- as.matrix(dtm)
# The DTM is stored as a Sparse Matrix by default. We convert it to a **regular matrix**.
# After conversion, it becomes a format that can be used to compute similarity between documents.

# Calculate Euclidean distance
distance_matrix <- dist(dtm_matrix, method = "euclidean")
# Euclidean distance calculates the **distance** (i.e., similarity) between each pair of documents.
# When two documents are similar, the distance is small, and when they are dissimilar, the distance is large.
# `method  "euclidean"` specifies that the Euclidean distance metric is used.

# Perform hierarchical clustering
cluster <- hclust(distance_matrix, method = "ward.D2")
# Hierarchical clustering is performed.
# The `hclust()` function uses the distance matrix obtained from `dist()` to perform hierarchical clustering.
# `method = "ward.D2"` is used for the clustering method (Ward’s method)

# Load the factoextra package
library(factoextra)

# Visualize the dendrogram (display the hierarchical clustering result as a tree)
cluster %>%
  fviz_dend(k=5,                    # Divide the dendrogram into 5 clusters
            show_labels = FALSE,    # Do not display labels for documents (for a cleaner view)
            rect = TRUE,            # Display rectangles around each cluster
            color_labels_by_k = TRUE) # Color the labels according to their cluster
# Similar to the above method, this is the result of clustering the Titles of each article.


# Convert the distance matrix to a regular matrix
distance_matrix <- as.matrix(distance_matrix)
# `distance_matrix` is stored as a `dist` object, so we convert it to a matrix to allow indexing and manipulation.

# Find the two documents with the smallest Euclidean distance
min_distance <- min(distance_matrix[distance_matrix > 0])  
# Find the smallest value in the `distance_matrix` that is greater than 0.
# `0` represents the distance from a document to itself, so we exclude it and focus on the actual distances between documents.

# Extract the indices of the documents corresponding to the minimum distance
closest_docs <- which(distance_matrix == min_distance, arr.ind = TRUE)[1, ]
# `which()` returns the index positions that satisfy the given condition.
# `distance_matrix == min_distance` finds the indices of the two documents with the smallest distance.
# `arr.ind = TRUE` returns the result in matrix index format. We select only the first position `[1, ]`.

# Indices of the two closest documents
doc1_index <- closest_docs[1]
doc2_index <- closest_docs[2]
# Store the indices of the two closest documents in `doc1_index` and `doc2_index`.

# Print the contents of the two documents
text_corpus[[doc1_index]]$content
text_corpus[[doc2_index]]$content
# Extract and print the actual content of the two closest documents based on their indices.
# Use `[[ ]]` to retrieve the specific document content and compare the two closest documents.



##################


# Create Document-Term Matrix (DTM)
dtm <- DocumentTermMatrix(text.set.clean)
# The DTM is a matrix representing the relationship between documents and words.
# It provides information on how often each word appears in each document.

dtm_matrix <- as.matrix(dtm)
# The DTM is stored as a Sparse Matrix by default. We convert it to a **regular matrix**.
# After conversion, it becomes a format that can be used to compute similarity between documents.

# Compute Euclidean distance
distance_matrix <- dist(dtm_matrix, method = "euclidean")
# Euclidean distance calculates the **distance** (i.e., similarity) between each pair of documents.
# When two documents are similar, the distance is small, and when they are dissimilar, the distance is large.
# `method = "euclidean"` specifies that the Euclidean distance metric is used.

# Perform hierarchical clustering
cluster <- hclust(distance_matrix, method = "ward.D2")
# Hierarchical clustering is performed.
# The `hclust()` function uses the distance matrix obtained from `dist()` to perform hierarchical clustering.
# `method = "ward.D2"` is used for the clustering method (Ward’s method)

# Load the factoextra package
library(factoextra)

# Visualize the dendrogram (display the hierarchical clustering result as a tree)
cluster %>%
  fviz_dend(k=5,                    # Divide the dendrogram into 5 clusters
            show_labels = FALSE,    # Do not display labels for documents (for a cleaner view)
            rect = TRUE,            # Display rectangles around each cluster
            color_labels_by_k = TRUE) # Color the labels according to their cluster

# Convert the distance matrix to a regular matrix
distance_matrix <- as.matrix(distance_matrix)
# `distance_matrix` is stored as a `dist` object, so we convert it to a **matrix** to allow indexing and manipulation.

# Find the two documents with the smallest Euclidean distance
min_distance <- min(distance_matrix[distance_matrix > 0])  
# Find the smallest value in the `distance_matrix` that is greater than 0.
# `0` represents the distance from a document to itself, so we exclude it and focus on the actual distances between documents.

# Extract the indices of the documents corresponding to the minimum distance
closest_docs <- which(distance_matrix == min_distance, arr.ind = TRUE)[1, ]
# `which()` returns the index positions that satisfy the given condition.
# `distance_matrix == min_distance` finds the indices of the two documents with the smallest distance.
# `arr.ind = TRUE` returns the result in matrix index format. We select only the first position `[1, ]`.

# Indices of the two closest documents
doc1_index <- closest_docs[1]
doc2_index <- closest_docs[2]
# Store the indices of the two closest documents in `doc1_index` and `doc2_index`.

# Print the contents of the two documents
text.set.clean[[doc1_index]]$content
text.set.clean[[doc2_index]]$content
# Extract and print the actual content of the two closest documents based on their indices.
# Use `[[ ]]` to retrieve the specific document content and compare the two closest documents.

