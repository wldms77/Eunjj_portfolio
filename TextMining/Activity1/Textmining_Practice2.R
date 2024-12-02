############################################
### Course: Text Mining                #####
### Subject: 2024-2                    #####
### Title: Practice 01                 #####
### TEAM: 04                           #####    
### Member: 21900471 Jeongin Yook      #####
###         22000294 Jemin   Park      #####
###         22100809 Eunji   Hwang     #####
############################################


setwd("~/Desktop/Textmining_Practice_team4/Textmining_Practice2_team4")
#Q.(1-1, 5 pts) Load “Trump_2021_final_speech.docx” to a variable called trump.speech. Using this variable, 
# generate a summarized data frame called trump.speech.sum. The first two rows are shown below.

#Import libraries
library('tidyr')
library('officer')

# Load Trump_2021_final_speech.docx and named as trump.speech 
Trump.speech <- read_docx('Trump_2021_final_speech.docx') 
# By using docx_summary(), extract the summary information form trump.speech
Trump.speech.sum <- Trump.speech %>% docx_summary
# Display the first two rows of the summary dataframe
Trump.speech.sum %>% head(2)


#Q. (1-2, 5 pts) In trump.speech.sum, how many doc_index are there? 
# What does the doc_index represent? Does it make sense?

# Check the content type ariable from the Trump.speech
length(Trump.speech.sum$text) # Check the length of text variable, it has 29 text
Trump.speech.sum$doc_index  # Check the length of doc_index variable,with 29

# Check the content type of data set, 
# and it shows the first 29 entries in the content_type column, all being labeled "paragraph". 
Trump.speech.sum$content_type 
# Using nrow(), check the number of rows of doc_index and content type
nrow(Trump.speech.sum[c('doc_index','content_type')])

## Explanation about docx_index :
#In the data-set, the doc_index variable appears to be an index or identifier that separates each phrase or paragraph in the speech.
#Since each of the 29 rows has its own doc_index value, it is presumed to be used for 
## the purpose of distinguishing individual paragraphs of the speech.
#In addition, since all content_type values are "paragraph", each doc_index represents a paragraph in the speech.
# this is a make-sense. When it is necessary to analyze each paragraph individually in a data-set 
# such as a speech, doc_index to distinguish it is very useful.



#Q.(1-3, 10 pts) Some parts of this speech file were not spoken by Trump. 
# Write down R codes to find those texts that include not human speaking texts. 
# Explain your codes. Expected results are shown below.

#install.packages('stringr')

#Import Library
library(stringr) 
# Extracts entries from the 'text' column of Trump.speech.sum that contain content within square brackets.
Trump.speech.sum$text[str_detect(Trump.speech.sum$text,"\\[.*\\]")] 
# .* means all 0 or more characters, which means any characters in brackets can be included.
# So we can find out cases that were not spoken by Trump
Trump.speech.sum$text

#Q (1-4, 10 pts) Assume that your main goal is to analyze Trump’s speech. 
# Update trump.speech.sum by removing texts that are not relevant to this goal.


# To remove any non-Trump speeches and retain only the texts where Donald Trump is the speaker,
# used grepl() to checks for any texts that start with(^) Donald Trump and \\[, \\] match literal square brackets
# Or, ! negates the result of any texts that inside square bracket and remove it. After 
Trump.speech.sum <- Trump.speech.sum %>%
  filter(grepl("^\\[Donald Trump\\]", text) | !grepl("\\[.*?\\]", text))

# Display the result that only Trump’s actual speeches remain
Trump.speech.sum$text


#Q.(1-5, 10 pts) How many times did Trump mention the word “love”?

# We assign Trump.speech.sum text column to new variable
count_love<-Trump.speech.sum$text

# Filter sentences that contain the word 'love'
counts_love <- count_love[gregexec('love',tolower(count_love))!='-1']

# Check if 'love' is present in counts_love (results in a logical vector)
grepl('love',counts_love)
grepl('love',tolower(counts_love))


# Iterate over each sentence in counts_love and check for 'love'
for (i in 1:length(counts_love)) {
  grepl('love',counts_love[i])
}

# Initialize total count of 'love'
total_love_count <- 0

# Loop through counts_love to find and count occurrences of 'love'
for (i in 1:length(counts_love)) {
  count <- gregexpr('love', tolower(counts_love[i])) # Find matches of 'love'
  print(regmatches(counts_love[i],count))            # Print matched occurrences
  total_love_count <- total_love_count + length(regmatches(counts_love[i], count)[[1]]) # Count occurrences and add to total
}

# Print the total count of 'love'
print(total_love_count)

 
#< Question 2 – AirBnB > (2-1, 10 pts) Import “airbnb.review.RData” to a variable called airbnb.review. 
# As a first step of data analysis, write down R codes that check whether there are duplication, outliers, 
# and missing value issues. Explain your reasoning.

# Load arbnb.review data
load("airbnb.review.RData") 

# Check structure, summary and print head of airbnb.review data 
airbnb.review %>% str
airbnb.review %>% head
airbnb.review %>% summary

# Check for duplicates
duplicates <- airbnb.review[duplicated(airbnb.review), ] 
num_duplicates <- nrow(duplicates) # Count the number of duplicate entries
# We can check there are not duplicated cases.

# Print the number of duplicate entries found
if(num_duplicates > 0) {
  cat("Number of duplicate entries:", num_duplicates, "\n") # Print the number of duplicate if there is duplicate 
} else {
  cat("No duplicate entries found.\n") # Print "no duplicate entries found" if there is no duplicate 
}

# Check for missing values in each column 
missing_values <- colSums(is.na(airbnb.review))
missing_values # Display the count of missing values for each column

# For listing_id, id, reviewer_id doesn't need to check outlier, we rather chose date to check outlier. 
airbnb.review$date_column <- as.Date(airbnb.review$date, format = "%Y-%m-%d") #change to date format 
summary(airbnb.review$date_column) #date is from "2009-03-30" to "2018-12-06" 
boxplot(airbnb.review$date_column) #it seems like there is no outlier 
# It would be hard to say the date as an outlier just because it is further than the average.

# (2-2, 10 pts) In airbnb.review, there are a total 11 listing_ids and 494 comments. 
# Create a list called airbnb.review.ls that each list element contains each listing_id’s comments. 
#  (ex. there should be 11 list elements) The first 15 words of 9 list elements are shown below.
library(dplyr)
#split the comments column by listing_id and create a list 
airbnb.review.ls <- split(airbnb.review$comments, airbnb.review$listing_id)

# Convert each comment into individual words and store them in the list
airbnb.review.ls <- lapply(airbnb.review.ls, function(comments) { 
  words <- unlist(strsplit(paste(comments, collapse = " "), " ")) # Split each comment into words and flatten them into a single list of words
  return(words)  # Return the words as a vector
})

# Set the names of the list to NULL
names(airbnb.review.ls) <- NULL

# Call the result using the provided method
lapply(airbnb.review.ls, head, 15)


# (2-3, 10 pts) From the expected results above, 
# you may find that some comments are not actually written by the reviewer, 
# but automatic messages. Update airbnb.review by removing automatic comments. 
# Also, update airbnb.review.ls. Expected results are shown below.

# Filter out rows where the 'comments' column contains the word "automated"
airbnb.review <- airbnb.review %>% filter(!str_detect(comments, "automated"))

# Split comments by listing_id and separate each comment into individual words
airbnb.review.ls <- lapply(split(airbnb.review$comments, airbnb.review$listing_id), function(comments) { #split the comments column by listing_id and create a list 
  words <- unlist(strsplit(paste(comments, collapse = " "), " "))   # Split each comment into words and flatten them into a single list of words
  return(words)  # Return the vector of words
})

# Set the names of the list to NULL
names(airbnb.review.ls) <- NULL

# Display the each listing_id's comments
lapply(airbnb.review.ls, head, 10)


# (2-4, 10 pts) Which listing_id is the most beloved by the reviewers? To answer this question, 
# write down R codes that count how many times the word “love” is mentioned for each listing_id in airbnb.review.ls.


combined_comments <- airbnb.review %>%
  group_by(listing_id) %>%
  summarise(combined_comments = paste(comments, collapse = " ")) 
# In airbnb.review, proceed to group by based on listening_id column.
# Then paste the commente of each listing_id using the summarise function.
# Create a variable called combined_comments and pass it.

combined_comments
# We want to check how many word 'love' are in each listing_id

combined_comments <- combined_comments %>%
  mutate(love_count = str_count(tolower(combined_comments), "love"))
# We add new column called love_count to check how many 'love' are in listing_id
# Using str_count function to count word and mutate function to add new column


combined_comments[order(combined_comments$love_count,decreasing=T),]
# With order function we can obtain this result
# I think long sentences tend to contain more words.



# (2-5, 10 pts) We are now interested in how love was being used 
# (or we may say that we want to know the text-context of love). 
# For only the case where the “love” is most frequently mentioned, 
# create a variable called love.neighbor that presents words mentioned before and after love. 
# The expected result of the first 6 rows of love.neighbor is shown below.

index<-order(combined_comments$love_count,decreasing=T)
# create index variable to get listing_id index
combined_comments<-combined_comments[order(combined_comments$love_count,decreasing=T),]
# And then sort combiined_commnet by love_count 

bf.af<-str_split(combined_comments$combined_comments,' ') 
# Split each listing_id's comment with space blank.


bf.af[[10]][c(which(str_detect(bf.af[[10]],'love'))-1,which(str_detect(bf.af[[10]],'love')),which(str_detect(bf.af[[10]],'love'))+1)]
# Find the word love of each listening_id through str_detect.
# Then, if you output it including the indexes before and after that, it comes out as follows.
bf.af[[1]][c(which(grepl('love',bf.af[[1]]))-1,which(grepl('love',bf.af[[1]])),which(grepl('love',bf.af[[1]]))+1)]
# Can use grepl to find all indexes in one comment in a similar way.


love.neighbor <- data.frame(
  listing_id = character(),  
  love.before = character(), 
  love = character(),        
  love.after = character())
# Create new dataframe contains 4 columns

for(i in 1:length(bf.af)){
  for(j in which(grepl('love',tolower(bf.af[[i]])))){
    love.neighbor<-rbind(love.neighbor,c(index[i],bf.af[[i]][c(j-1,j,j+1)]))
  }
}
# Using incorporate statement.
# Find all cases, including before and after the index where the love word is detected, 
# and add them to the data frame using the rbind function.

#love.neighbor <- rbind(colnames(love.neighbor), love.neighbor)
colnames(love.neighbor)<-c('listing_id','love.before','love','love.after')
# Convert column name as expected result

love.neighbor %>% head
# we can check the result

# (2-6, 10 pts) Which words were most frequently used before and after “love”? 
# As you could have seen from love.neighbor, love.before and love.after contain some meaningless symbols.
# To focus on the words, remove them and present the table of frequency.

love.neighbor$love.before
sort(table(love.neighbor$love.before),decreasing=T)
sort(table(love.neighbor$love.after),decreasing=T)
# Make a table to check how many words there are in love.after and love.before.
# The table was constructed in the order of the most used words using the sort function.

# before love case
love_before_freq <- sort(table(love.neighbor$love.before), decreasing = TRUE)
# Create new table that contain word frequency sorted

barplot(love_before_freq, 
        main = "Frequency of love.before", 
        xlab = "Words", 
        ylab = "Frequency", 
        col = "blue", 
        border = "black")
# It can be seen that the frequency of use is different for each word.

love_before_freq_filtered <- love_before_freq[love_before_freq >= 4]
# In order to exclude words that have no meaning due to the small number of uses, 
# only those with a frequency of use of 4 or more were filtered.

# It can be seen that the alphabet a is the most common after-love word.

barplot(love_before_freq_filtered, 
        main = "Words Mentioned 4 or More Times", 
        xlab = "Words", 
        ylab = "Frequency", 
        col = "blue", 
        border = "black")
# You can check five words that are used more than four times.

# after love case
love_after_freq <- sort(table(love.neighbor$love.after),decreasing=T)
# In a similar way, create a table with the word frequencies of love.after in order and pass it to the new variable.

barplot(love_after_freq, 
        main = "Frequency of love.after", 
        xlab = "Words", 
        ylab = "Frequency", 
        col = "red", 
        border = "black")
# It can be seen that the frequency of use is different for each word.

love_after_freq_filtered <- love_after_freq[love_after_freq >= 4]
# In order to exclude words that have no meaning due to the small number of uses, 
# only those with a frequency of use of 4 or more were filtered.

barplot(love_after_freq_filtered, 
        main = "Words Mentioned 4 or More Times", 
        xlab = "Words", 
        ylab = "Frequency", 
        col = "red", 
        border = "black")
# It can be seen that the word 'and' is the most common after-love word.


# You can check five words that are used more than four times.
# In the previous case, meaningful words may not be clear because words related to love are viewed comprehensively.
# So we decided to break down each word and analyze which word can be meaningful.

sort(table(love.neighbor$love),decreasing=T)
lovely_case<-love.neighbor[love.neighbor$love=='lovely',]
sort(table(lovely_case$love.before),decreasing=T)
sort(table(lovely_case$love.after),decreasing = T)
barplot(sort(table(lovely_case$love.before),decreasing=T)
, main = "Lovely Words Mentioned Top", 
        xlab = "Words", 
        ylab = "Frequency", 
        col = "red", 
        border = "black")
# You can check the before cases where the lovely word comes out separately.

barplot(sort(table(lovely_case$love.after),decreasing = T), 
        main = "Lovely Words Mentioned Top", 
        xlab = "Words", 
        ylab = "Frequency", 
        col = "blue", 
        border = "black")
# You can check the after cases where the lovely word comes out separately.

loved_case<-love.neighbor[love.neighbor$love=='loved',]
sort(table(loved_case$love.before),decreasing=T)
sort(table(loved_case$love.after),decreasing = T)
barplot(sort(table(loved_case$love.before),decreasing=T)
        , main = "Loved Words Mentioned Top", 
        xlab = "Words", 
        ylab = "Frequency", 
        col = "red", 
        border = "black")
# You can check the before cases where the loved word comes out separately.

barplot(sort(table(loved_case$love.after),decreasing = T), 
        main = "Loved Words Mentioned Top", 
        xlab = "Words", 
        ylab = "Frequency", 
        col = "blue", 
        border = "black")
# You can check the after cases where the loved word comes out separately.

love_case<-love.neighbor[love.neighbor$love=='love',]
sort(table(love_case$love.before),decreasing=T)
sort(table(love_case$love.after),decreasing = T)
barplot(sort(table(love_case$love.before),decreasing=T)
        , main = "Love Words Mentioned Top", 
        xlab = "Words", 
        ylab = "Frequency", 
        col = "red", 
        border = "black")
# You can check the before cases where the love word comes out separately.

barplot(sort(table(love_case$love.after),decreasing = T), 
        main = "Love Words Mentioned Top", 
        xlab = "Words", 
        ylab = "Frequency", 
        col = "blue", 
        border = "black")
# You can check the after cases where the love word comes out separately.

# It can be seen that the patterns of words before and after are different for each word, such as lovely, lovely, and love. 
# Through this, meaningful words may be different for each word.

