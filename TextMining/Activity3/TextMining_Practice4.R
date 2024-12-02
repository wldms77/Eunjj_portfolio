############################################
### Course: Text Mining                #####
### Subject: 2024-2                    #####
### Title: Practice 04                 #####
### TEAM: 04                           #####    
### Member: 21900471 Jeongin Yook      #####
###         22000294 Jemin   Park      #####
###         22100809 Eunji   Hwang     #####
############################################


## < Question 1 – David > ##
# Referring to the description below, answer the following questions:

# Import necessary libraries 
library(officer)
library(dplyr)
library(tidytext)
library(tidyr)
library(stringr)
library(ggplot2)

## Q1-1. Import “David.pptx” into a variable called david in R by using functions from the “officer” package. 
## With david, create a variable called david.content, a summarized data.frame. Expected results are shown below.

# By using read_pptx(), read the PowerPoint file into 'david' 
david <- read_pptx("~/TextMining_R/TextMining R/Practice 4/David.pptx")

# Extract the content summary of the PowerPoint using 'pptx_summary()',
# which will create a data frame that contains a summary of the slides and elements
david.content <- pptx_summary(david)

# Display the first row of 'david'
david.content %>% head(1)


## Q 1-2. Assume that we want to analyze the texts about David and his Psalms. Create a variable
## called david.content.all that contains only non-empty descriptive texts about David and his Psalms.
## The expected dimension of david.content.all is (6, 5).


# Filter the content to keep only the rows that the 'text' column contains a period '.'
david.content.all<-david.content[str_detect(david.content$text,'\\.'),] 

# Remove rows where the 'text' column is NA (missing values)
david.content.all<-david.content.all[!is.na(david.content.all$text),]

# Check the dimension of 'david.content.all, which is (6, 5)
dim(david.content.all)



## Q 1-3. To analyze the whole description texts, 
## create a vector called david.all as shown below.

# Combine all the text from the 'text' column into a single character string 
# using paste() with collapse argument to join entries with a space
david.all <- paste(david.content.all$text, collapse = " ")

# Display the first 20 characters of the combined text string by substr()
david.all %>% substr(1,20)


## Q 1-4. Create a unigram, bigram and trigram token frequency tables and compare their top 10 lists. 
## Assuming that there will be no further text pre-processing, 
## which case do you think is better for text analysis? Explain.

# Define a function that removes punctuation and extra spaces from the input text
clean_text <- function(text) {
  text %>%
    str_remove_all("[[:punct:]]") %>% # remove punctuation
    str_squish()                      # remove extra whitespace
}

# Apply the function that we made in the previous step,to the combined text and store them
cleaned_text <- clean_text(david.all)

# Create unigrams rom the cleaned tex
david.unigrams <- data.frame(text = unlist(strsplit(cleaned_text, "\\s+"))) %>% # Split text into words
  count(text, sort = TRUE) %>%   # Count the frequency 
  rename(Freq = n)               # Rename the count column to 'Freq'

# Create bigrams from the cleaned text
david.bigrams <- data.frame(text = unlist(strsplit(cleaned_text, "\\s+"))) %>%
  mutate(bigram = paste(text, lead(text))) %>%   # Create bigrams by combining each word with the next
  filter(!is.na(bigram)) %>%                     # Remove rows with NA values
  count(bigram, sort = TRUE) %>%                 # Count the frequency 
  rename(Freq = n)                               # Rename the count column to 'Freq'


# Create trigrams from the cleaned text
david.trigrams <- data.frame(text = unlist(strsplit(cleaned_text, "\\s+"))) %>%
  mutate(trigram = paste(text, lead(text), lead(text, 2))) %>%  # Create trigrams by combining each word with the next two
  filter(!is.na(trigram)) %>%                                # Remove rows with NA values
  count(trigram, sort = TRUE) %>%                            # Count the frequency 
  rename(Freq = n)                                           # Rename the count column to 'Freq'


# Extract the top 10 most frequent used text of each - grams 
top_unigrams <- head(david.unigrams, 10)
top_bigrams <- head(david.bigrams, 10)
top_trigrams <- head(david.trigrams, 10)

# Display the results
print(top_unigrams)
print(top_bigrams)
print(top_trigrams)

# Explanation : which case is better for text analysis ###
# Looking at each result, first of all, Unigram is certainly easy to understand 
# because the frequency value is very high, but there is a limitation that words that do not have practical meaning, 
# such as the, of, and to, belong.Next, look at the Bigram, the frequency value is lower than the unigram, 
# but it is helpful to understand it in terms of context. Last, The last Trigram is good to understand the context, 
# but it is a little difficult to understand because the highest frequency is 3.
# As a result, we think 'unigram or birgram' is good for text analysis. 
# This is because it is the most convincing in terms of frequency-based analysis.




## Q 1-5. Create a tri- and quadgram token frequency table and generate a bar plot of the top 20 
## as shown below. From this result, what sort of text pre-processing tasks should be conducted? 
## Write down your suggestions and explain why.


# Create a data frame for trigrams 
david.trigrams <- data.frame(text = unlist(strsplit(cleaned_text, "\\s+"))) %>%
  mutate(trigram = paste(text, lead(text), lead(text, 2))) %>%  # Create trigrams
  filter(!is.na(trigram)) %>%                                   # Remove rows with NA values
  count(trigram, sort = TRUE) %>%                              # Count the frequency
  rename(Freq = n)                                             # Rename the count column to 'Freq'


# Create a data frame for quadgram 
david.quadgrams <- data.frame(text = unlist(strsplit(cleaned_text, "\\s+"))) %>% 
  mutate(quadgram = paste(text, lead(text), lead(text, 2), lead(text, 3))) %>% # Create quadgram
  filter(!is.na(quadgram)) %>%                                                 # Remove rows with NA values 
  count(quadgram, sort = TRUE) %>%                                             # Count the frequency
  rename(Freq = n)                                                             # Rename the count column to 'Freq'
 

# Combine trigrams and quadgrams into a single data frame
combined <- david.trigrams %>%
  rename(phrase = trigram) %>%         # Renamed as phrase 
  mutate(type = "trigram") %>%         # Add a column to indicate type: 'trigram
  bind_rows(david.quadgrams %>% 
              rename(phrase = quadgram) %>%    # Rename 'quadgram' to 'phrase'
              mutate(type = "quadgram"))       # Add a column to indicate type: 'quadgram'
 

# Summarize the combined data frame to get the total frequency of each phrase
top_combined <- combined %>% 
  group_by(phrase) %>%  # Group by phrase
  summarise(Freq = sum(Freq)) %>%  # Sum the frequencies by each phrase
  arrange(desc(Freq)) %>%          # Arrange by frequency with desc()
  slice_head(n = 20)               # Select the top 20 phrases


# Create a bar plot for the top 20 phrases 
ggplot(top_combined, aes(x = reorder(phrase, Freq), y = Freq)) +
  geom_bar(stat = "identity") +
  coord_flip() +    # Flip coordinates for better readability
  labs(title = "Top 20", x = "Phrase", y = "Frequency")   # Add title and labels


## Explanation : text pre-processing tasks ##
# 1) Remove Stop Words : Remove common stop words such as "is," "the," and "of." 
# These words occur frequently but don’t carry significant meaning, and their removal helps focus on more important terms.
# 2) Lemmatization/Stemming: For doing Lemmatization, considers the context and part of speech of the word. 
# For stemming, by cutting a suffix from a word and converting it into a word that is close to its root. 
# But, the important thing is both techniques should ensure that word meanings are not distorted, 
# and steps are needed to ensure that the converted results accurately reflect the meaning of the text.
# 3) Lowercasing (exception: ie, Hebrew, David, Gospel, ect.) : Analyzing the text by converting it to lowercase letters. 
# However, be careful of texts that need to maintain the shape of capital letters such as Hebrew and Gospel.



# < Question 2 – Simpson >
#   Referring to the description below, answer the following questions:
#   (2-1, 5 pts) Import “simpsons_text.csv” to a variable called simpson, “simpsons_locations.csv” to a
# variable called simpson.loc, and “simpsons_characters.csv” to a variable called simpson.char in R. In
# case of simpson, rearrange by “episode_id” and “character_id”. Expected results are shown below.
# install.packages('tm')
# library(tm)
library(officer)
library(dplyr)
options(encoding='UTF-8')
# Import the library to use and proceed with encoding.


# simpsons_text.csv
simpson<-read.csv("~/Desktop/Textmining_Practice_team4/Textmining_Practice4_team4/Simpsons/simpsons_text.csv")
simpson<-simpson %>%arrange(id) 
simpson %>% head
# Import the simpsons_text.csv file into the read_csv function and sort by id. 
# Check the results.


# simpson.loc
simpson.loc<-read.csv("~/Desktop/Textmining_Practice_team4/Textmining_Practice4_team4/Simpsons/simpsons_locations.csv")
simpson.loc %>% head
# Imports the simpsons_locations.csv file as a read_csv function.
# Check the results.

# simpson.char
simpson.char<-read.csv("~/Desktop/Textmining_Practice_team4/Textmining_Practice4_team4/Simpsons/simpsons_characters.csv")
simpson.char %>% head
# Imports the simpsons_characters.csv file as a read_csv function.
# Check the results.

# (2-2, 10 pts) Which location was being used most frequently in each episode? Generate a data.frame
# type variable called simpson.top.loc that can show each episode’s most frequently used location. The
# first six rows of three columns (episode_id, name and count) are shown below.

simpson.top.loc <- simpson %>%
  group_by(episode_id, location_id) %>% # Use the groupby function based on episode_id, location_id.
  summarise(count = n(), .groups = 'drop') %>%# Use the summer feature to count each group and release the group through drop.
  arrange(episode_id, desc(count)) %>% # Sort based on episode_id and ensure that the highest count comes first.
  group_by(episode_id) %>% # Regroup the group with episode_id.
  slice(1) %>% # Gets only the first value so that you can only get the value with the largest count.
  left_join(simpson.loc, by = c("location_id" = "id")) %>% # Keep all the rows of the simpson.loc data frame, and take and combine the matching values from other data frames.
  select(episode_id, name, count) # select columns we want to see


simpson.top.loc %>% head(20) # check the result

# (2-3, 15 pts) Let’s try something similar with character instead of location. Which character appears
# the most in each episode? For this operation, 1) create a new data.frame called simpson.ed, which
# contains the new columns called “character” and “script”. Here, both of them are made from
# “raw_text”. 2) Using simpson.ed, generate a data.frame type variable called simpson.top.char that can
# show each episode’s most frequently appeared character. Expected results are shown below.
library(stringr)
library(dplyr)
library(tidyr)
library(stringr)

#1) 

simpson.ed <- simpson %>% mutate(
  character = ifelse(str_detect(raw_text, "^\\(.*\\)$"), "", str_split(raw_text,":") %>% sapply(`[`, 1)), 
  script = ifelse(str_detect(raw_text, "^\\(.*\\)$"), raw_text, str_split(raw_text, ":") %>% sapply(`[`, 2))
)
# Use the ifelse function to type "" if raw_text starts with "()", words are included between them, and then split on the basis of " otherwise:". 
# Then, use the apply function to assort the first and second values of the separated values using the character and script using the mute function.
simpson.ed %>% head
# check the result

#2) 


simpson.top.char <- simpson.ed %>%
  group_by(episode_id, character) %>% # Use the groupby function based on episode_id, character.
  summarise(count = n(), .groups = 'drop') %>%# Use the summer feature to count each group and release the group through drop.
  arrange(episode_id, desc(count)) %>% # Sort based on episode_id and ensure that the highest count comes first.
  group_by(episode_id) %>% # Regroup the group with episode_id.
  slice(1) %>% # Gets only the first value so that you can only get the value with the largest count.
  select(episode_id, character, count) # select columns we want to see

# Check the reuslt of simpson.top.char 
print(head(simpson.top.char))

# (2-4, 10 pts) For this assignment, we are only interested in what characters said, not how they acted.
# As you can see from the “script” column, any texts written inside the parentheses are action not a
# dialogue. Create a new column called “script_ed” that does not contain actions. Comparison of
# “script” and “script_ed” is shown below
simpson.ed$script %>% head(10)


# Add a script_ed column to the simpson.ed
simpson.ed <- simpson.ed %>%
  mutate(
    script_ed = str_remove_all(script, "\\(.*?\\)") %>%  # Delete parentheses and the contents in them.
      str_trim() # Remove the first and last spaces through the trim function.
  ) # create new column with mutate function called script_ed


simpson.ed$script_ed %>% head(10)
# Check the result

# (2-5, 10 pts) What are the keywords of each episode? To answer this question, generate a list called
# simpson.ep. The elements of this list are texts of each episode
simpson.ep <- simpson.ed 
# Load the dataset into simpson.ep

simpson.ep %>% select(episode_id,script_ed) 
# Select specific columns from simpson.ep 

simpson.ep <- simpson.ed %>%
  group_by(episode_id) %>%
  summarise(script_ed = paste(script_ed, collapse = " ")) 
# Group the simpson.ed dataset by episode_id and collapse the script_ed values 

simpson.ed%>%head()
# Check the first 6 rows

simpson.ep <- list() # Initialize an empty list 
for(i in 1:length(unique(simpson.ed$episode_id))){
  simpson.ep[[i]] <- simpson.ed[simpson.ed$episode_id==i,"script_ed"] %>% 
    .[.!=""] %>% # Remove empty strings from the script lines
    unlist %>%    # Convert the selected script lines into a vector
    paste(collapse = " ")  # Concatenate the vector into a single string for each episod
}
# View the first 6 characters 
simpson.ep[[1]] %>% head()
# Check the number of episodes processed
simpson.ep %>% length


##### Based on the above simpson.ep, we want to identofy the keywords of each episode####

install.packages("stopwords")
library(stopwords)
stop_words <- stopwords::stopwords(source = "nltk")
# assing stopwrods to find keywords except for meaningless words.

simpson.ep$script_clean <- sapply(simpson.ep$script, function(x) {
  # Remove punctuation in script
  x <- gsub("[[:punct:]]", "", x)
  
  # Convert to lowercase to check keywords
  x <- tolower(x)
  
  # Split the text into words and choose first element
  words <- str_split(x, "\\s+")[[1]]
  
  # Remove stopwords
  words_clean <- words[!words %in% stop_words]
  
  # Recombine the cleaned words into text
  paste(words_clean, collapse = " ")
})

simpson.ep <- as.data.frame(simpson.ep)
class(simpson.ep)
str(simpson.ep)

word_count <- simpson.ep %>%
  mutate(words = strsplit(script_clean, "\\s+")) %>%  # Split by whitespace
  unnest(words) %>%  # Flatten the list into rows
  count(words, sort = TRUE)  # Count word frequencies

word_count[-1,] %>% head
# so we can check meaningful words in simpson script except stopwords.


# (2-6, 15 pts) Using simpson.ep, generate a VCorpus variable called simpson.cleaned. This should be a
# text pre-processed corpus. For instance, removing punctuations, numbers, whitespaces, stopwords,
# converting to lowercase, performing lemmatization and tokenization, etc. should be conducted.
# Explain your text pre-processing steps and reasons.
install.packages('textstem')
library(textstem)
library(dplyr)
library(tm)

simpson.cleaned<-VCorpus(DirSource("~/Desktop/Textmining_Practice_team4/Textmining_Practice4_team4/Simpsons"))
summary(simpson.cleaned)
simpson.cleaned[[1]]$content %>% head(10)
# With tm library function we import corpus to deals with multiple documents systematically.


dot_tokenizer <-function(x) gsub(",","",x)
# In simplepson.cleaned[[1]] it could be seen that the values were divided based on . 
# Therefore, after erasing the letters, the corresponding function was created to process preprocessing.

simpson.cleaned <- simpson.cleaned %>%
  tm_map(removePunctuation) %>% 
  tm_map(removeNumbers) %>% 
  tm_map(stripWhitespace) %>% 
  tm_map(stemDocument) %>% # 
  tm_map(content_transformer(lemmatize_strings)) %>% 
  tm_map(content_transformer(tolower)) %>% 
  tm_map(removeWords, stopwords("en")) %>%  
  tm_map(content_transformer(dot_tokenizer))

simpson.cleaned[[1]]$content%>% head

# 1. removePunctuation: Removes all punctuation from the text to eliminate unnecessary symbols that could affect analysis.
# 2. removeNumbers: Removes numeric characters to prevent numbers from influencing the text analysis in irrelevant ways.
# 3. stripWhitespace: Cleans up the text by removing unnecessary spaces 
# 4. stemDocument: Reduces words to their root form (stem) to normalize variations of the same word.
# 5. lemmatize_strings: Converts words to their base form based on context, making them more meaningful.
# 6. tolower: Converts all text to lowercase to ensure consistency in text processing and avoid case-sensitive discrepancies.
# 7. removeWords: Removes stopwords (common words like 'the', 'is') that do not add meaningful information for analysis.
# 8. dot_tokenizer: Replace , to "" blank to onbtain pure text








