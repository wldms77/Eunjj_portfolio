#####Import necessary Libraries####
library(dplyr)
library(tidyr)
library(KoNLP)
library(wordcloud2)
library(stringr)
library(rvest)
library(dplyr)
library(stringr)


# set the URL of petition board site 
base_url <- "https://cheongwon.go.kr/portal/petition/open/view?pageIndex=%d&searchType=1&slideYn=Y&searchBgnDe=2024-01-01&searchEndDe=2024-11-30&dselectw=6&searchPtnTtl=&searchPtnCn=&searchInstNm=&rlsStts=01%%2C02%%2C03&rlsSttsArr=01&rlsSttsArr=02&rlsSttsArr=03&type=card"

petition_data <- data.frame(page_id = integer(), title = character(), text = character(), label = character(), stringsAsFactors = FALSE)
# 
# # Petition text extract 

# Repeat to extract the text by 1 ~ 30 page 
for (i in 1:151) {
  # Create the URL of each pages 
  url <- sprintf(base_url, i)
  
  # Get the Web page 
  web_page <- read_html(url)
  
  # Extract the title of Petiotn 
  petition_title <- web_page %>%
    html_nodes(".subject") %>%  # Found span element with class 'subject'
    html_attr("title")          # Extract 'title' properties
  
  petition_text <- web_page %>%
    html_nodes(".text") %>%     # Found span element with class 'text'
    html_text(trim = TRUE)      # Extract text and remove spaces
  
  
  # To extract a category class of petitions
  petition_label <- web_page %>%
    html_nodes(".category") %>% # Found span element with category' class
    html_text(trim = TRUE)      # Extract text and remove spaces
  
  # Add results to data frames
  page_data <- data.frame(page_id = rep(i, length(petition_title)),
                          title = petition_title,
                          text = petition_text,
                          label = petition_label,
                          stringsAsFactors = FALSE)
  
  # Comvine the Data frame 
  petition_data <- bind_rows(petition_data, page_data)
}
petition_data$page_id %>%unique()

# Extract the top 10 labels(Institution name)
top_labels <- petition_data %>%
  count(label, sort = TRUE) %>%
  slice_head(n = 10) %>%
  pull(label)
unique(petition_data$label)

top_labels <- petition_data %>%
  count(label, sort = TRUE) %>%
  mutate(percentage = n / sum(n) * 100) %>%
  slice_head(n = 10)  # Only select the top 10 labels 




# 2. Defined the stopwords(Kor) 
stopwords <- c("그리고", "하지만", "또한", "정말", "하는", "위해", "하여", "있다", "없다", letters, LETTERS, as.character(0:9))
stopwords <- unique(c(stopwords, intToUtf8(44032:55203, multiple = TRUE), letters, LETTERS, as.character(0:9)))

# Keyword Analysis Function by Label

analyze_keywords <- function(label, petition_data, stopwords) {
  useSejongDic()  # Sejong 사전 사용
  
  # Filtered the Label 
  filtered_data <- petition_data %>%
    filter(label == !!label)  
  
  # Extract the Key words of Title 
  title_keywords <- filtered_data %>%
    mutate(
      clean_text = sapply(title, \(x) str_replace_all(x, "[^가-힣\\s]", "") %>% str_squish()),
      nouns = sapply(clean_text, extractNoun),
      keywords = sapply(nouns, \(x) x[!x %in% stopwords])
    ) %>%
    unnest(keywords) %>%
    count(keywords, sort = TRUE)
  
  # Extract the Key words of Text 
  text_keywords <- filtered_data %>%
    mutate(
      clean_text = sapply(text, \(x) str_replace_all(x, "[^가-힣\\s]", "") %>% str_squish()),
      nouns = sapply(clean_text, extractNoun),
      keywords = sapply(nouns, \(x) x[!x %in% stopwords])
    ) %>%
    unnest(keywords) %>%
    count(keywords, sort = TRUE)
  
  list(label = label, title_keywords = title_keywords, text_keywords = text_keywords)
}

#Import the library which analysis Korean morpheme 
library(KoNLP)

# Enable keyword analysis for all labels
all_results <- lapply(top_labels, analyze_keywords, petition_data = petition_data, stopwords = stopwords)

# 5. Output results and create word clouds
for (result in all_results) {
  cat("Label:", result$label, "\n")
  
  # Display the top 10 key words of Title 
  cat("Top Title Keywords:\n")
  print(result$title_keywords %>% slice_head(n = 10))
  
  #  Display the top 10 key words of Text 
  cat("Top Text Keywords:\n")
  print(result$text_keywords %>% slice_head(n = 10))
  
  # Create Title Word Cloud
  wordcloud2(
    data = result$title_keywords %>% slice_head(n = 50),  # Top 50 Key words 
    size = 1,
    color = "random-light",
    backgroundColor = "white"
  )
  
  # Create Text Word Cloud 
  wordcloud2(
    data = result$text_keywords %>% slice_head(n = 50),  # Top 50 Key words 
    size = 0.6,
    color = "random-light",
    backgroundColor = "white"
  )
  
  cat("\n\n")
}
all_results %>% str





#---------------------------LDA modeling -----------------#
library(KoNLP)      
library(tm)           
library(topicmodels)  
library(tidytext)     
library(tidyverse)   

# Text preprocessing function
preprocess_text <- function(text, stopwords) {
  # Remove non-Korean characters
  text <- str_replace_all(text, "[^가-힣\\s]", "") %>%
    str_squish()
  
  # 2) Extract the Noun 
  nouns <- extractNoun(text)
  
  # 3) Remove stopwords 
  nouns <- nouns[!nouns %in% stopwords]
  
  # 4) Rejoin as text
  paste(nouns, collapse = " ")
}

# Defined the stopwords 
stopwords <- c("그리고", "하지만", "있다", "합니다", "때문에", "대한", "정도", "로부터", "까지", "통해",
               "합니다", "하고", "너무", "더", "있습니다", "왜", "저는" ,"이런", "많이", "정말 ",
               "및", "대한", "년", "경우", "등", "관련", "또는", "등의", "같은", "있는",
               "이", "가", "을", "를", "의", "에", "에서", "에게", "으로", "로", "와", "과", "도", "만", "까지", "처럼", "만큼", "보다", "부터", "한테", 
               "에서", "의해서", "으로부터", "또는", "그래서", "그런데", "그러므로", "그러니까", "그럼", "만약", "그렇다면", "따라서", "왜냐하면", "왜냐면", "그저", "그리하여", "또한",
               "많은", "새로운", "좋은", "큰", "작은", "아름다운", "빠른", "느린", "긴", "짧은", "높은", "낮은", "높은", "깨끗한", "더운", "차가운", "어두운", "밝은", "어려운", "쉬운",
               "많이", "잘", "너무", "그렇게", "아주", "조금", "더", "덜", "별로", "이렇게", "어떻게", "진짜", "가끔", "언제나", "어떤", "다시", "먼저", "일찍", "늦게", "심하게", "급하게")



# Shortening Long Text to Process
petition_lda <- all_results %>%
  mutate(clean_text = sapply(text, function(x) {
    x <- substr(x, 1, 1000)  # do it up to 1000 characters
    preprocess_text(x, stopwords = stopwords)
  }))



# 2. Text- Pre processing 
petition_lda <- all_results %>%
  mutate(clean_text = sapply(text, preprocess_text, stopwords = stopwords))

# 3. Create the DTM 
corpus <- Corpus(VectorSource(petition_lda$clean_text)) # Create corpus
dtm <- DocumentTermMatrix(corpus, 
                          control = list(wordLengths = c(2, Inf))) # Only contain words with 2 or more characters

# Rename the row of dtm matrix 
rownames(dtm) <- 1:nrow(dtm)


petition_data <- petition_data %>%
  distinct(clean_text, .keep_all = TRUE)

#remove the sparse matrix 
dtm <- removeSparseTerms(dtm, 0.98)


#remove the ocuments without words as 0
library(slam)
row_totals <- row_sums(dtm)
sum(row_totals == 0)  
dtm1 <- dtm[row_totals > 0, ]


# 5. LDA Modeling 
set.seed(1234) # Fixed seeding for reproducibility
lda_model <- LDA(dtm1, k = 5, control = list(seed = 1234)) # set k=5 as topic number  

# 6. Display the result 
topics <- tidy(lda_model, matrix = "beta") # Extract word weight by topic
top_terms <- topics %>%
  group_by(topic) %>%
  slice_max(beta, n = 10) %>% # Choose the top 10 words from each topic
  ungroup()

print(top_terms) #check the result 





#------------------Analyzing by Label---------------#
#install.packages("NLP4kec")
#install.packages("/Library/Frameworks/R.framework/Versions/4.4-arm64/Resources/library/java/NLP4kec_1.4.0.tgz", repos = NULL)

# 1) combine Title_keywords and text_keywords for each label in the data
all_results_clean <- lapply(all_results, function(label_data) {
  #Extract and combine keywords columns from Title_keywords and text_keywords
  title_keywords <- label_data$title_keywords$keywords
  text_keywords <- label_data$text_keywords$keywords
  
  # Combine keywords into one text
  combined_text <- paste(c(title_keywords, text_keywords), collapse = " ")
  
  # Converting text to data frames
  data_frame(label = label_data$label, text = combined_text)
})




##2) Use text keyword only 
all_results_clean <- lapply(all_results, function(label_data) {
  # Extract keywords column from text_keywords
  text_keywords <- label_data$text_keywords$keywords
  
  # Combine text into one string
  combined_text <- paste(text_keywords, collapse = " ")
  
  # Converting text to data frames
  data_frame(label = label_data$label, text = combined_text)
})



# Create text-combined data frames for all labels in the list
all_results_clean_df <- bind_rows(all_results_clean)
all_results_clean_df %>% head(1)

# List of Korean terminology
stopwords_ko <- c("그리고", "하지만", "있다", "합니다", "때문에", "대한", "정도", "로부터", "까지", "통해",
                  "합니다", "하고", "너무", "더", "있습니다", "왜", "저는", "이런", "많이", "정말",
                  "및", "대한", "년", "경우", "등", "관련", "또는", "등의", "같은", "있는", "이", "가", 
                  "을", "를", "의", "에", "에서", "에게", "으로", "로", "와", "과", "도", "만", "까지", 
                  "처럼", "만큼", "보다", "부터", "한테", "에서", "의해서", "으로부터", "또는", "그래서", 
                  "그런데", "그러므로", "그러니까", "그럼", "만약", "그렇다면", "따라서", "왜냐하면", "왜냐면", 
                  "그저", "그리하여", "또한", "많은", "새로운", "좋은", "큰", "작은", "아름다운", "빠른", 
                  "느린", "긴", "짧은", "높은", "낮은", "높은", "깨끗한", "더운", "차가운", "어두운", "밝은", 
                  "어려운", "쉬운", "많이", "잘", "너무", "그렇게", "아주", "조금", "더", "덜", "별로", 
                  "이렇게", "어떻게", "진짜", "가끔", "언제나", "어떤", "다시", "먼저", "일찍", "늦게", "심하게", 
                  "급하게", "수", "그", "하는", "할", "따른", "피해를", "한다", "할", "관한","가거나", "위한", 
                  "위해", "한", "현재","따라", "없는","제조", "생각합니다", "경우에는", "것이", "있도록", "청원",
                  "때에는", "것입니다", "것은", "것", "모든", "지금", "각", "아닌", "다", "해당", "본인이", "중",
                  "다른", "사람이", "아이를","제항의","때","받을","것으로","것을","사항","필요한","없이", "없습니다","같이","았을",
                  "다음", "지원", "해주세요", "개편바랍니다", "인력에", "관해", "심각하다", "문제가", "원수다", "벙실", "함부로",
                  "못쓰게", "살인고문행위", "신문언론들", "문제", "그리고", "아니라", "될","인해",
                  "제가", "일을", "등을", "안녕하세요", "바랍니다", "모두", "이러한", "있을", "있다고", "입니다", "국민의",
                  "세","등을", "제", "가장", "있고", "한다고", "특히", "안", "후", "또", "그러나", "만원",
                  "국가", "해야","저희", "이상", "않고", "있어", "우리", "매우", "되었습니다", "대해", "매년", "얼마나", "될까요", "힘든건", "사실",
                  "힘이","힘듭니다","힘좀","힘이되시길","힘없는", "힘써야", "힘써주심에", "힘을","힘들어하는","힘들지", "않는",
                  "혜택을", "하지", "않습니다","지원을", "이는", "되는", "더욱", "대한민국", "있는데", "국민들이", "사망한", 
                  "부탁드립니다", "밖에", "돈을", "다양한","정부","이미","받고","알고","못하고","다만","계속","해서","하며",
                  "제대로","직접","있으며","각종","가지고","하는데","있게","정부의","국민","월",
                  "어느", "필요합니다","인한","의사가","연금을","않은","사회적","받지","바로","내",
                  "것이다", "같습니다","청원을","임차인이","범죄를","차량","제항의","제항에","해당하는","시간",
                  "법을","사형을","된","제조의","제조에","정하는","청원합니다","아니한","이하의","등에","등등","연구",
                  "한국","이를","함께","개정을","호의","제항","받은","제호","육아휴직을","불구하고","법은","시","서울시","교육","공부","해외",
                  "학교","살", "년간", "대","것도","하게","없고","국민투표로진행해주세요", "돌보면서","가입을","자격증을","큽니다","선택으로해야합니다")

# (1) Text preprocessing: tokenization and terminology removal
cleaned_text <- all_results_clean_df %>%
  unnest_tokens(word, text) %>%
  filter(!word %in% stopwords_ko)  # 불용어 제거

print(cleaned_text, n = 50)


## (2) Select a label and preprocess text
cleaned_text_health <- all_results_clean_df %>%
  filter(label == "보건복지부") %>% # select the lable : 보건복지부
  unnest_tokens(word, text) %>%
  filter(!word %in% stopwords_ko) %>%
  filter(!word %in% stopwords_ko) %>%  
  filter(!str_detect(word, "^힘")) %>% #Remove a word that begins with a specific word
  filter(!str_detect(word, "^히")) %>%
  filter(!str_detect(word, "^희")) %>%
  filter(!str_detect(word, "흘")) %>%
  filter(!str_detect(word, "^흔")) %>%
  filter(!str_detect(word, "^훌"))


# Sort in descending order and output the top 30
cleaned_text %>%
  arrange(desc(word)) %>% 
  print(n = 15)
cleaned_text_health 


library(ggplot2)
# Visualize the top 20 words and show the each labels 
plot <- ggplot(cleaned_text %>%
                 count(label, word, sort = TRUE) %>%
                 top_n(20, n), aes(x = reorder(word, n), y = n, fill = label)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme(text = element_text(family = "AppleGothic")) +  # set the Korean font 
  labs(title = "Top 20 Words per Label")

plot # Display




# Check the frequency of each word
term_frequency <- colSums(as.matrix(dtm))
head(sort(term_frequency, decreasing = TRUE), 10)

dtm <- cleaned_text_health %>%
  count(label, word) %>%
  cast_dtm(label, word, n, weighting = tm::weightTf)

# Filtering words that are too rare or have appeared too many times
dtm <- dtm[, colSums(as.matrix(dtm)) > 5]  # Remove words that appear less than 5 times
lda_topics <- tidy(lda_model, matrix = "beta")
lda_topics_sorted <- lda_topics %>%
  arrange(desc(beta))
lda_topics_sorted %>% head(20)



## Visualizie with wordcloud and figure out which term is most frequently used
library(wordcloud2)

# After word extraction in DTM, create a frequency-based word cloud
word_freq <- data.frame(word = colnames(dtm), freq = colSums(as.matrix(dtm)))
#Display the plot 
wordcloud2(word_freq, size = 0.5, color = 'random-light', backgroundColor = 'black',par(family = "AppleGothic"))













