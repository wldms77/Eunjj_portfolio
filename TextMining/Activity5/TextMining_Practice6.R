############################################
### Course: Text Mining                #####
### Subject: 2024-2                    #####
### Title: Practice 06                 #####
### TEAM: 04                           #####    
### Member: 21900471 Jeongin Yook      #####
###         22000294 Jemin   Park      #####
###         22100809 Eunji   Hwang     #####
############################################

# Import necessary libraries
library(jsonlite) 
library(dplyr) 
library(stringr)
library(tidyr) 
library(tidytext)
library(purrr)
library(igraph)
library(ggraph)
library(scriptuRs)
library(ggplot2)

## Question (1-1, 5 pts) Import data.json from the URL (http://data.nasa.gov/data.json) 
# and store the data into a variable called nasa (Hint: use jsonlite::fromJSON).
# For your convenience, save this data as a RData file with the name, nasa.RData.

nasa <- fromJSON('https://data.nasa.gov/data.json') #download data using fromJSON 
#save(nasa, file = "~/Desktop/Textmining_Practice_team4/Textmining_Practice6_team4/nasa.RData") #save it in nasa.RData 
names(nasa) 


## Question (1-2, 10 pts) In nasa, you can get set data by selecting “dataset” column. Create a data.frame called nasa.keyword.df, 
# which contains only two columns called id (dataset’s identifier) and keyword (dataset’s keyword).
names(nasa$dataset) 
nasa.keyword.df <- nasa$dataset %>% select(id = identifier, keyword = keyword) #select identifier column as id and keyword column as keyword 
head(nasa.keyword.df)


## Question (1-3, 10 pts) Using nasa.keyword.df, create a new variable called nasa.keyword.pair, a pair matrix of keyword.
# Your pair matrix should contain three columns of keyword1, keyword2, and frequency.
# Remember that our goal is to analyze the keyword structure of NASA project.
nasa.keyword.pair <- nasa.keyword.df %>% 
  unnest(keyword) %>% #flatten the keyword column 
  group_by(id) %>% #group by id 
  filter(n() > 1) %>% #filter the id that has more than 1 keyword becouse we are making keyword pair 
  summarise(keyword_pairs = list(combn(keyword, 2, simplify = FALSE))) #by using combination function, make all possible combinations of keywords

nasa.keyword.pair %<>% 
  unnest(keyword_pairs) %>% #flatten the keyword_pairs column 
  mutate(
    keyword1 = map_chr(keyword_pairs, ~ min(.x)), #by using map_char function in each pair, select the minimum value as keyword1 
    keyword2 = map_chr(keyword_pairs, ~ max(.x))) %>% #select the maximum value as keyword2, by doing this we can avoid the duplication of keyword pairs
  select(-id, -keyword_pairs) #delete id and keyword_pairs column 

nasa.keyword.pair %<>% 
  group_by(keyword1, keyword2) %>% #group by keyword1 and keyword2 
  summarise(frequency = n()) %>% #count the frequency of each keyword pair 
  arrange(desc(frequency)) #arrange in descending order


## Question (1-4, 15 pts) Check at least top 50 rows of nasa.keyword.pair to see if there’s any unacceptable keywords. 
# If necessary, update nasa.keyword.pair, if not, leave as it is.

nasa.keyword.pair %>% print(n=100) #check the top 100 rows of keyword pair 
nasa.keyword.pair[26,] #since "ngda" is the abbreviation of "national geospatial data asset", this keyword pair does not needed 
nasa.keyword.pair <- nasa.keyword.pair[-26,] #delete this keyword pair 


## Question (1-5, 10 pts) Using nasa.keyword.pair, create a network graph. 
# Remember that the network analysis graphics should be intuitive and easy to understand. 
# You may do any additional operations that you think it is necessary. 
# All your operations and network graphics should be logically making sense.

network <- nasa.keyword.pair %>% 
  filter(frequency > 500) %>% #since there are too many keyword pairs, select only the keyword pairs that have more than 500 frequency
  graph_from_data_frame(directed = FALSE) #make a igraph object 

V(network)$word_frequency <- degree(network) #add the degree of vertex as word_frequency 
E(network)$pair_frequency <- E(network)$frequency #add the attribute of edge as pair_frequency 

ggraph(network, layout = "fr") + #show the graph using ggraph 
  geom_edge_link(aes(width = pair_frequency), color = "gray", alpha = 0.7) +  #set the edge width by pair_frequency 
  geom_node_point(aes(size = word_frequency), color = "skyblue") +  #set the vertex size by word_frequency 
  geom_node_text(aes(label = name), repel = TRUE, size = 3) +  # Add labels to the vertex 
  scale_edge_width(range = c(0.5, 3)) +  # Scale edge width
  theme_void() + # Remove the background 
  ggtitle("NASA Keyword Network Graph") 
#through the result, we can know "earth science" is the most frequent keyword 




## < Question 2 – Psalms > ##

##Question (2-1, 5 pts) Import the King James Bible by using scriptuRs:: kjv_bible() function.
# Here, we want to analyze the book of Psalms. Check the data set and create a variable called psalms. 
# Select the list of columns that you think they are necessary.

#install.packages('scriptuRs')

library(scriptuRs)
psalms <- kjv_bible()
# Import scriptuRs library to import the kjv_bible dataset.

psalms %>% head
psalms %>% names
psalms %>% nrow
# Let's check the overall contents of psalms.

psalms %>% sapply(unique) %>% sapply(length)
# The number of psalms columns can be expressed as follows. 
# In order to identify the contents of the text, such as two volume_id, 66 book_id, and each chapter_id, all values must be unique with the following contents.

psalms_a<-psalms %>% select(volume_id,book_id,chapter_id,chapter_number,verse_id,text)
psalms %>% select(verse_title,text)
# Take this particular column, make it possible to identify the content, and pass it to the variable psalms_a.


## Question(2-2, 5 pts) Run a simple space tokenization without text pre-procesing and count the frequency of words. 
# For this task, use functions from tidytext package.

#install.packages('tidytext')

# Import tidytext library
library(tidytext)

# Group the data by book_id and chapter_id
psalms_grouped <- psalms_a %>%
  group_by(book_id, chapter_id)

# Ungroup the data and tokenize the text into individual words, then calculate word frequencies
tokens <- psalms_grouped %>%
  ungroup() %>%  # Remove grouping to perform operations on the entire dataset
  unnest_tokens(word, text) %>%  # Tokenize the 'text' column into individual words
  count(book_id, chapter_id, word, sort = TRUE)  # Count the frequency of each word within book_id and chapter_id

# Extract the most frequent word in each chapter based on frequency
most_frequent_words <- tokens %>%
  group_by(book_id, chapter_id) %>%  # Group data by book_id and chapter_id
  slice_max(n = 1, order_by = n) %>%  # Select the word with the highest frequency for each group
  ungroup()  # Remove grouping to return a flat dataframe

# Check thee result
most_frequent_words %>% print(n=50)
most_frequent_words$word %>% unique

##Explanation: ##
# If we look at the words that appeared the most in the 50 chapters, you can see that they are mostly composed of words that correspond to stopwords, such as the, and.
# Of the approximately 1,200 textual contents, just over 20 words appeared the most.
# This requires a text preprocessing process to remove stopwords.


## Question (2-3, 10 pts) Does overall tone of the book of Psalms positive or negative?
# To answer this question, run sentiment analysis, compare frequency and proportion of words.
# Do you think the result makes sense? Explain.

# Load the Bing sentiment lexicon for positive/negative sentiment analysis
bing_lexicon <- get_sentiments("bing")

# Tokenize Psalms text and count the frequency of positive and negative words
psalms_tokens <- psalms_a %>%
  unnest_tokens(word, text) %>%  # Split text into individual words (tokens)
  mutate(word = tolower(word)) %>%  # Convert all words to lowercase for consistency
  inner_join(bing_lexicon, by = "word") %>%  # Match tokens with the Bing sentiment lexicon
  count(sentiment, sort = TRUE)  # Count the frequency of positive and negative words

# Display the tokenized and sentiment-matched word counts
psalms_tokens

# Calculate the proportion of positive and negative sentiments in the text
sentiment_proportions <- psalms_a %>%
  unnest_tokens(word, text) %>%  # Split text into individual words (tokens)
  mutate(word = tolower(word)) %>%  # Convert all words to lowercase for consistency
  inner_join(bing_lexicon, by = "word") %>%  # Match tokens with the Bing sentiment lexicon
  count(sentiment) %>%  # Count the frequency of positive and negative words
  mutate(proportion = n / sum(n))  # Calculate the proportion of each sentiment type relative to total sentiment words

# Display the calculated sentiment proportions
sentiment_proportions

# Create a bar plot visualizing the proportion of positive and negative sentiments
ggplot(sentiment_proportions, aes(x = sentiment, y = proportion, fill = sentiment)) +
  geom_bar(stat = "identity") +  # Create bars representing sentiment proportions
  labs(title = "Sentiment Analysis of Psalms", x = "Sentiment", y = "Proportion") +  # Add titles and axis labels
  theme_minimal()  # Apply a minimal theme for a clean visual appearance

##Explanation: ##
# When word emotion analysis was performed with the Bing dictionary, 
# it can be seen that overall negative words appeared more than positive words.
# However, given that the distribution of the two types of words is not so different, the specimen is generally very rich in human emotions, 
# and it is difficult to define a specific tone because positive (admiration and appreciation) and negative (difficulty and petition) elements are mixed in a balanced way.


## Question (2-4, 10 pts) Let’s check the tone for each chapter of Psalms. 
# To do so, propose a new variable called “sentiment.index”, which can explicitly compares the number of positive words and negative words. 
# Explain how you created this variable.


data("stop_words") # Load the stop words dataset

# Tokenization and preprocessing of text
tokens <- psalms_a %>%
  unnest_tokens(word, text) %>%  # Tokenize the text into words
  mutate(word = tolower(word)) %>%  # Convert all words to lowercase
  anti_join(stop_words, by = "word") %>%  # Remove stop words
  count(book_id, chapter_id, word, sort = TRUE)  # Count frequency of each word per chapter

# Extract the most frequent keyword from each chapter
most_frequent_keyword <- tokens %>%
  group_by(book_id, chapter_id) %>%    # grouped book_id and chapter_id
  slice_max(n = 1, order_by = n) %>%  # Extract the most frequent word from each chapter
  ungroup()

# Display the result of most frequent keyword 
most_frequent_keyword


# Combine book_id and chapter_id into a new column called 'book_chapter'
most_frequent_keyword <- most_frequent_keyword %>%
  mutate(book_chapter = paste(book_id, chapter_id, sep = "-"))

#Load the Bing sentiment lexicon
bing_lexicon <- get_sentiments("bing")  #Sentiment classification for positive and negative words

# Join the 'most_frequent_keyword' data with the Bing sentiment lexicon
most_frequent_keyword_sentiment<-most_frequent_keyword %>% full_join(bing_lexicon,by='word')

#Replace NA sentiment values with 'Netural' (neutral) for words that are not found in the lexicon
most_frequent_keyword_sentiment$sentiment[is.na(most_frequent_keyword_sentiment$sentiment)]<- 'Netural'
most_frequent_keyword_sentiment # Display the result 


# Create a sentiment index for each chapter
# calculates the sentiment index for each chapter by summing the positive sentiment words 
# and subtracting the count of negative sentiment words.
sentiment_index_summary <- most_frequent_keyword_sentiment %>%
  group_by(book_id, chapter_id) %>%
  summarise(
    sentiment.index = sum(sentiment == "positive") - sum(sentiment == "negative"), 
    .groups = "drop"
  )

#  Add the sentiment.index to the original data
most_frequent_keyword_sentiment <- most_frequent_keyword_sentiment %>%
  left_join(
    sentiment_index_summary, 
    by = c("book_id", "chapter_id") # Merge the sentiment index by chapter
  )

# Display the result 
most_frequent_keyword_sentiment %>% print(n = 100) # n = 200 / n = 300... (increase the number of print n size, then we can see the sentiment index + / - too.)

## Explaination of how we created this variable : 
# Bing_lexicon contains a list of words categorized as positive or negative, 
# Each word in the most_requent_keyword dataset was matched with bing_lexicon to confirm whether it has positive or negative emotions. 
# Then, to calculate the sentiment index, the number of positive and negative words was counted for each chapter 
# (identified as book_id and chapter_id), and the next sentiment index was calculated as the difference between the number of positive and negative words. 
# If words were not classified in bing_lexicon, neutral sentiment was given and did not contribute to the emotion index calculation.



## Question (2-5, 10 pts) What are the top 5 positive and negative words in Psalms? 
# Are they align with what you know about the Psalms? Explain.


# Get the top 5 most frequent positive words
top5_positive_words <- most_frequent_keyword_sentiment %>%
  filter(sentiment == "positive") %>% # Filter for positive sentiment
  group_by(word) %>%      # Group by word to count occurrences
  summarise(n = sum(n)) %>%
  arrange(desc(n)) %>%    # Sort words by frequency in descending order
  head(5)     # Select the top 5 words

# Display the result 
top5_positive_words


# Get the top 5 most frequent negative words
top5_negative_words <- most_frequent_keyword_sentiment %>%
  filter(sentiment == "negative") %>% # Filter for positive sentiment
  group_by(word) %>%         # Group by word to count occurrences
  summarise(n = sum(n)) %>%
  arrange(desc(n)) %>%    # Sort words by frequency in descending order
  head(5)       # Select the top 5 words

# Display the result 
top5_negative_words

##Explanation : the top 5 positive words are faith, gold, praise, mercy, and angel 
# the top 5 negative words are unclean, wicked, sin, evil, fool. 
# we think, top 5 positive words align with the themes commonly found in the Psalms
# It reflect themes of trust in God, gratitude, and divine protection, which are central in the Psalms.
# Also, for negative top 5 words, which are frequently addressed in the Psalms, often through calls for repentance and divine justice.




## Question (2-6, 10 pts) Create a wordcloud that compares the “positive” and “negative” sentiments based on the frequency. 
# With this result, what can you tell about the book of Psalms?

# Install and load necessary libraries
#install.packages("wordcloud")
library(wordcloud)
library(RColorBrewer)

# Extract positive words and remove duplicates
positive_words <- most_frequent_keyword_sentiment %>%
  filter(sentiment == "positive" & !is.na(n) & n > 0) %>%  # Filter for positive words with frequency greater than 0
  group_by(word) %>%  
  summarise(n = sum(n)) %>%  # Sum frequency of duplicates
  ungroup()  # Ungroup after summarizing

# Create a word cloud for positive words
wordcloud(words = positive_words$word,    
          freq = positive_words$n,      # Frequency of each word
          min.freq = 1,           # Min frequency
          max.words = 100,        # Maximum number of words to display
          colors = brewer.pal(8, "Blues"),  # Color palette for the word cloud
          scale = c(3, 0.5),      # Word size range
          random.order = FALSE)  



# Extract negative words and remove duplicates 
negative_words <- most_frequent_keyword_sentiment %>%
  filter(sentiment == "negative" & !is.na(n) & n > 0) %>% # Filter for negative words with frequency greater than 0
  group_by(word) %>%
  summarise(n = sum(n)) %>%   # Sum frequency of duplicates
  ungroup()   # Ungroup after summarizing

# Create a word cloud for neative words
wordcloud(words = negative_words$word, 
          freq = negative_words$n,   # Frequency of each word
          min.freq = 1,     # Min frequency
          max.words = 200,  # Max frequency
          colors = brewer.pal(8, "Reds"), # Color palette for the word cloud
          scale = c(3, 0.5),   # Word size range
          random.order = FALSE) 

## Explanation : ## 
# When looking at the positive words in the psalm, words such as faith, praise, and mercy are most prominent in the table. 
# It can be seen that the relationship between humans worshiping and dedicating themselves to God is emphasized.
# After that, you can see love words, and there are words such as wisdom and salute. 
# Therefore, I think there are many positive words related to relationships, such as heart and posture that humans give to God.

# When looking at negative words, words related to sin, such as unclean, evill appear large. 
# It can be seen that it reveals a lot of negative emotions about the sins committed by humans. 
# Next, words such as blind and poor are seen, and words related to human defects are related.
# Therefore, it shows a lot of human flaws, and it seems that we must have a desire for salvation with God's mercy.



# Question (2-7, 10 pts) What are the top 5 sentiments word that has been used most frequently in Psalms? 
# To answer this question, measure the frequency of sentiment words on average. 
# From the result, find which chapter used the most frequently used sentiment word the most.


# Load 'nrc' sentiment dictionary
nrc_lexicon <- get_sentiments("nrc")

# Tokenize text data and match sentiment words
sentiment_analysis <- psalms_a %>%
  unnest_tokens(word, text) %>%  # Tokenize text word by word
  mutate(word = tolower(word)) %>% # Converting words to lowercase
  inner_join(nrc_lexicon, by = 'word') %>%  # Matching with 'nrc'
  group_by(book_id, chapter_id, sentiment) %>%  # Group by each chapter sentiments
  summarize(sentiment_word_count = n()) %>%  # Calculate the number of each sentiment words
  ungroup()

# Find the most frequently used sentiment words for each chapter
most_common_sentiment_chapter <- sentiment_analysis %>%
  group_by(book_id, chapter_id) %>%  # Group by chapter
  filter(sentiment_word_count == max(sentiment_word_count)) %>%  # Choose the most common sentiment
  select(book_id, chapter_id, sentiment, sentiment_word_count) %>%   #Select the necessary information 
  ungroup()

# Print the most frequently used sentiment for each chapter
print("most frequently used sentiment for each chapter정:")
print(most_common_sentiment_chapter)

# Check how many of the most frequently used sentiment words appeared in each chapter
most_common_sentiment_word_count <- psalms_a %>%
  unnest_tokens(word, text) %>%  # Tokenized text word by word
  mutate(word = tolower(word)) %>%
  inner_join(nrc_lexicon, by = 'word') %>%  # Matching with 'nrc'
  inner_join(most_common_sentiment_chapter, by = c("book_id", "chapter_id", "sentiment")) %>%  # Matching the most popular emotions in each chapter
  group_by(book_id, chapter_id, sentiment) %>%  # Grouping
  summarize(total_sentiment_word_count = n()) %>%  # Calculate the number of corresponding emotion words
  ungroup()
most_common_sentiment_word_count

# Display the result to verify which chapter used sentiment words most 
most_common_sentiment_word_count %>% arrange(desc(total_sentiment_word_count))

##Explanation: 
# First, the reason for using the nrc dictionary instead of the bing dictionary is that in the case of the bing dictionary, 
# it is classified only as positive or negative, so it is difficult to know the detailed word type.
# In the case of nrc, it was considered suitable for detailed analysis because the detailed emotion types of 
# 10 basic emotions (trust, joy, anger, fear, etc.) combined with positive and negative belong. 

# As can be seen from the results, the most frequently used emotional word is positive, 
# and the average frequency is also high. 
# It can be seen that this was used the most in Chapter 597. 
# Furthermore, it can be seen that the specimen contains many positive and hopeful messages.


