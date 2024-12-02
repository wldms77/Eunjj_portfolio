############################################
### Course: Text Mining                #####
### Subject: 2024-2                    #####
### Title: Practice 03                 #####
### TEAM: 04                           #####    
### Member: 21900471 Jeongin Yook      #####
###         22000294 Jemin   Park      #####
###         22100809 Eunji   Hwang     #####
############################################


#Import libraries 
library("rvest")
library("stringr")
library("dplyr")
library("ggplot2")
library("pdftools")
library("tidyr")

# < Question 1 – Act 19 >
#   Referring to the description below, answer the following questions:
## Q 1-1. Nowadays, we can easily read Bible verses through the Internet. Create a variable called
# html that contains all HTML text from the link below. (Hint: Use rvest package)

html<-read_html('https://www.biblegateway.com/passage/?search=Acts%2019&version=NIV')
html
# load html dataset with read_html function and assign it to html variable

## Q 1-2. How many <div> tags are included in html? Extract all <div> tags that have a class
# containing 'text-html' to a variable called div_lists. Expected results are shown below.
div_lists <- html_nodes(html,'div')
# Gets values consisting of div tags.
length(div_lists)
# Obtain length of list with length function

div_lists <- html_nodes(html, "div[class*='text-html']") 
# Extract the HTML nodes from the 'html' object with 'div', where the class attribute includes the sub-string 'text-html'
div_lists
# Check the result

## Q 1-3. If you check the given website, all the bible verses are written in <p> tags. Extract all <p>
# tag texts to a variable called ptag. Expected results are shown below

ptag <- html_nodes(div_lists,'p')
# Gets values consisting of 'p' tag and assign it to ptag

ptag
# Check the result

## Q 1-4. From ptag, extract all the texts to a variable called act.19. Expected results are shown below. 
# How many elements are there in act.19? What does the number of elements in act.19 indicate?

act.19<-html_text(ptag)
# obtain texts from nodes we have gotten.
act.19 %>% head(2)
# check results with head function.

## Q 1-5. Below is the screenshot of the website. As you can see, some unnecessary texts are
# included. Remove foot note, verse number and unexpected texts created from html_text 
#(ex. (A), (B), etc.), and update act.19. In the updated act.19, elements should be the Bible verse. 
# (Punctuations will be removed in the later)

act.19 %>% str_extract_all("\\([:alpha:]{1,}\\)")
# Extracts when there is more than one alphabet between parentheses
# We can find words we want to remove with this process. 

a <- str_remove_all(act.19, "\\([:alpha:]{1,}\\)|\\[[:alpha:]{1,}\\]")
# Using remove function. we find cases that are more than one alphabet between square and circle bracket with | (or)
# And assign it to a variable

b <- paste(a[1:14], collapse = "") %>% str_split('[:digit:]') 
#Combine using the paste function from 1 to 14 in variable a and divide by word based on space. 
#After No. 14, metadata that is different from the contents of the Bible comes out, so we will exclude it.

act.19 <- lapply(b,function(x)x[x!=""]) %>% unlist
# except case which is empty word with lapply function and unlist it
act.19%>%head
# and check results with head function

## Q 1-6. Use act.19 to create a data.frame called act.19.df, which contains two columns of
# verse_number and verse refer to the verse number and verse text respectively. Expected results are shown below.

len_act<-1:length(act.19)
# Use length function to make index of act.19 and assign it to variable
act.19

act.19.df <-data_frame(
  verse_number=len_act,
  verse=act.19
)
# create a new data frame called act.19.df and assign two columns, index and contents

act.19.df<-as.data.frame(act.19.df)
# we use as.data.frame function to make our data frame as expected result

act.19.df %>% 
  head(2)
# check the result.

## Q 1-7. Use act.19.df to create a vector called act.19.word. The elements of this vector should be
# pre-processed words (lower-case and no punctuations).
act.19.df$verse
act.19.word<-str_remove_all(act.19.df$verse ,"[:punct:]")%>% 
  str_replace_all("\\s{2,}", " ")%>% tolower%>%str_split(" ") %>% unlist
#Use remove function to delete punctuation and replace all characters with lower alpha.
#Then, use the replace function to replace two or more spaces with one space, and then use the split function to divide them into space units. 
#Unlist will also proceed.

act.19.word%>% head
# check our result


## Q 1-8. Use act.19.word to create a data.frame called act.19.word.df, which presents each word’s
# frequency of being mentioned in Act 19. Expected results are shown below.

act.19.word <- str_replace_all(act.19.word, "\\s+", "")
# Remove all whitespace from the act.19.word

uniq_word<-sort(unique(act.19.word))
# obtain act.19.word's unique words and sort it by a,b,c alphabet. Assign it to new variable

aa<-c()
# create empty vector

length(act.19.word)
# check length of act.19.word

for (i in 1:length(uniq_word) ){
  aa[i]<-sum(act.19.word==uniq_word[i])
  # count how many word repeated in there 
}
# Use iterative corporation to create new vector that contain frequency of unique word
# Use unique words to count how many frequencies each word has in the entire data and then pass it to the i-th value of the aa vector.

act.19.word.df<-data_frame(word=uniq_word,
                           Freq = aa) %>% as_data_frame
# Create a data frame and enter how many times each word has been repeated in act.19.word.

act.19.word.df <- as_data_frame(act.19.word.df)
# Use as_data_frame to make it as expected value

act.19.word.df<-act.19.word.df[-1,]
# " (blank) is the value that comes first when done in order.
# We get the rest of the data frames except because we don't need to check them in this case.

# Display the result 
act.19.word.df%>% head

# Q 1-9. To find out the words that are most frequently used in Act 19, create a bar graph of Top
# 30 words as shown below. Do you think this result is helpful? Explain.

top30 <- act.19.word.df %>% arrange(desc(Freq)) %>% slice(1:30)
# Get the top 30 rows and sorted by 'Freq' in descending order.

top30
#Display the result 

ggplot(top30, aes(x = reorder(word, Freq), y = Freq)) + 
  geom_bar(stat="identity") + coord_flip() 
# Pull the top 30 cases based on the number of word frequencies and then pass them to a new variable called top30. 
# Visualize and check the result value of top30 through ggplot.

# If you look at the upper list of words in top 30, it mainly contains values such as the, of, and.
# It is difficult to say that the word has a special meaning just because it is repeated often.
#I think it would be a good way to calculate the frequency by finding meaningful words such as Paul, except for words that do not have such meaning.


# Q 2-1. When selecting a major, it is important to know the faculty. 
# The link below presents the faculty members of the School of Applied Artificial Intelligence. 
# Create a variable called saai_html that contains all HTML text from the link below.

saai_html <- read_html("https://www.handong.edu/kor/major/college/ACE/faculty/")
# bring information from designated url by using read_html function 

saai_html
# Display the result 

# Q 2-2. From saai_html, extract all the information related to faculty members 
# and store it into a new variable called saai. Expected results are shown below.

saai <- saai_html %>%
  html_nodes("table") %>%  # select table tags from saai_html 
  html_table() # convert html table in data frame 

saai
# Display the result 

# 2-3 Create a data.frame called saai.df, 
# which presents all faculty members of SAAI with their name, major, office, phone number, and e-mail address. 
# Expected results are shown below.

saai.df <- data.frame(Name = character(), Major = character(), Office = character(), Phone = character(), Email = character())
# made a data frame with only column names 
for (i in saai){
  new <- data.frame(Name = i$X2[1], Major = i$X2[2], Office = i$X2[3], Phone = i$X2[4], Email = i$X2[5])
  # using for loop, get the elements from each table's X2 column and store them in a new data frame 
  saai.df <- rbind(saai.df, new) 
  # combine a new data frame into saai.df data frame. 
}
saai.df 
# Display the result 

# Q. 2-4 Update saai.df as shown below: 
# Create new columns called KorName and EngName from Name column.

saai.df <- separate(saai.df, 'Name', into = c('Kor', 'Eng'), sep = "\\(") 
# using separate function, separate Name by ( into Kor and Eng 
saai.df$Eng <- str_remove_all(saai.df$Eng,'[:punct:]') 
# remove all the other punctuation in Eng column 

saai.df
# Display the result 

## Q 3-1. Import Matthew10.pdf to a variable called mat10. As you noticed, it has two character vectors. 
# Update mat10 to have a single element.

mat10 <- pdf_text("Matthew10.pdf")
# Extract text from the pdf file and store it in 'mat10'
mat10
# Print the full content 

mat10 <- paste(mat10, collapse = "")
# Update 'mat10' to a single string by concatenating all pages using paste() with collapse

mat10 %>% str 
# Display the structure of the 'mat10' object to verify it is now a single string


## Q 3-2. From mat10, Create a verse vector called mat10.verse. 
# The elements of mat10.verse should be each verse. Expected results are shown below.
# Split the text into verses based on the presence of double newlines or specific punctuation

mat10.verse <- str_split(mat10, "\\d+") %>% # Split 'mat10' into verses using digits (\\d+) sequences as delimiters
  lapply(function(x) str_replace(x, "^\\n", ""))
  # Remove leading newline characters (\\n) from the beginning of each verse (denoted by ^)

mat10.verse <- mat10.verse[[1]][mat10.verse[[1]] != ""]
# Extract the cleaned verses, filtering out any empty elements

mat10.verse
# Display the resulting verses


## Q 3-3. From mat10.verse, 
# create a variable called mat10.verse.word.mat10.verse.word contains list of words for each verse. 
# Expected result of the first list element is shown below. Here, make sure it only contains the words.

mat10.verse.word <- 
  mat10.verse %>% tolower() %>%  # convert all verse to lowercase
  str_replace_all("[\\s*\n\\s*[:punct:]]", " ") %>%   
  # Replace any white space (spaces, tabs), newline(\\n), or punctuation with a single space
  str_trim() %>%  
  # Trim leading and trailing white space from each verse
  str_split("\\s+")
# Split the cleaned verses into words based on one or more white space characters

mat10.verse.word[[1]]
# Display the the first verse words


## Q 3-4. What are the most frequently used word in Matthew 10? 
# To answer this question, 
# 1) generate a data.frame that shows list of top 10 words and their frequency, and 
# 2) bar plots. Explain whether your finding make sense or not.

# 1)
# Create a data frame of the top 10 most frequent words from the verse words
top10 <- 
  mat10.verse.word %>% unlist() %>%  # Flatten the list of words into a single vector
  table() %>%  # Create a frequency table of the words
  data.frame() %>%  # Convert the table to a data frame 
  arrange(desc(Freq)) %>%  # Arrange the data frame in descending order of frequency
  slice(1:10) # Select the top 10 rows

colnames(top10) <- c("word", "Freq") 
# Rename the columns 

top10 
# Display the result 
# We can see that the most frequently used words are 'the', 'and', and 'of', etc.

# 2)
# Create a bar plot of the top 10 most frequent words
ggplot(top10, aes(x= reorder(word, Freq), y= Freq)) + 
  geom_bar(stat = "identity") + # Use geom_bar to create a bar plot with the frequency of each word
  coord_flip() 
# Flip the coordinates to make it a horizontal bar plot

# To explain the result of  top 10 most frequent words that make sense or not, 
# The reason for this result is that these words are very commonly used to construct sentences in English. 
# Although they don't carry significant meaning on their own, they frequently appear, 
# which explains this outcome, and it makes sense. 
# However, when performing text analysis, it is more useful to remove words that do not contribute much meaning 
# and focus on analyzing words with substantial meaning.


## Team 4 New approach on Question 3-4 ##
# Words such as the, of, and are not words with real meaning.
# we tried by removing the terminology and extracting only noun words that have practical meaning with the library 'tm'

#install necessary package 
install.packages("tm")

# Import 'tm' library
library(tm)


# Convert the text data into a VCorpus (volatile corpus) object
# VCorpus() is a collection of text documents in memory
corpus <- VCorpus(VectorSource(mat10))

# Text preprocessing on the corpus
corpus <- corpus %>%
  tm_map(content_transformer(tolower)) %>%  # Convert text to lowercase
  tm_map(content_transformer(removePunctuation)) %>% # Remove punctuation
  tm_map(content_transformer(removeNumbers)) %>%  # Remove numbers
  tm_map(removeWords, c(stopwords("en"), "will")) %>% # Remove stopwords and the word 'will'
  tm_map(removeWords, c("“", "”", "’", "‘", "\"", "'")) %>%  # Remove unnecessary symbols
  tm_map(stripWhitespace)  # Remove extra whitespace


tdm <- TermDocumentMatrix(corpus)
# Create a term-document matrix from the corpus
matrix <- as.matrix(tdm) 
# Convert the term-document matrix to a standard matrix
word_freqs <- sort(rowSums(matrix), decreasing = TRUE)
# Calculate and sort word frequencies

# Convert the word frequencies to a data frame, remove spaces and duplicates
word_freqs_df <- data.frame(word = names(word_freqs), freq = word_freqs) %>%
  filter(word != "") %>%  # Remove empty strings
  mutate(word = trimws(word)) %>%  # Remove leading and trailing spaces from words
  distinct(word, .keep_all = TRUE)  # Remove duplicate words while keeping all other columns

# Extract the top 10 most frequent words
top_words_new <- head(word_freqs_df, 10)



# Plot a bar graph of the top 10 words and their frequencies
ggplot(top_words_new, aes(x = reorder(word, -freq), y = freq)) + 
  geom_bar(stat = "identity", fill = "steelblue") + # Create a bar plot with steelblue color
  theme_minimal() +  # Use a minimal theme for the plot
  labs(title = "Words that Most Frequently Used in Matthew 10", 
       x = "Words", 
       y = "Frequency") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Rotate x-axis labels for readability

## After removing the stopwords, the new result of the most frequently used in 'mat10' is  
# 'father', 'one', 'welcomes', and 'whoever', which each appear 6 times in the text. 
# So that the particular words play a significant role in this chapter. 
