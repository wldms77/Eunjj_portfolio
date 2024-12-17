# Analysis of Public Needs through Text Mining based on National Petition Board

*This project was conducted as a part of “Text Mining” class Fall 2024 Semester in Handong Global University under Prof. Keungoui Kim.* 

---

## Team Member

- Jeongin Yook / https://github.com/jeongin777
- Jemin Park / https://github.com/tori4913
- Eunji Hwang / https://github.com/wldms77

---

## This repository contains the following folders and projects:

- Data
    - **data_crawling.R** : Code file for performing HTML tag-based scrolling on the Petition Board24 website.
    - **petition_data.RData**: File storing the crawled text data in RData format.
- R code
    - **TextPreprocessing.R**: Code file for preprocessing the data stored in the RData file.
    - **Term Frequency & N-gram.R**: Code file for conducting term frequency and N-gram analysis to extract key keywords.
    - **LDA.R**: Code file for applying LDA to identify and visualize major topics.

---

## Project Introduction

Petition Board is run by Korea Government and serves as a platform for people to voice their concerns and request action on various issues. 

Common petitions involve demands for harm relief, the correction of injustices, and even requests for changes in laws or public services. 

One notable example of the board’s impact is that A powerful case in point is the Gangseo PC Room Murder Incident from 2018. Following this tragic event, a petition gained over 1.19 million signatures, leading to the passage of the Kim Seong-su Law, which addressed issues related to public safety. 
This shows the significant real-world effects that petitions on this platform can have. 

---

## Project Objective

- Extract Key Words of Each Department and Perform LDA Topic Modelling
- Find Inherent Needs of Public and Make a Suggestion to Legislator

---

## Data

Source : National Petition Board 

URL : https://www.cheongwon.go.kr/portal/petition/open/view 

Web Crawled Date : 1 December 2024 

Date : January 2023 – November 2024

Data Content : 2,953 observations / 5 variables: "page_id", "title", "text", "label", "date"

---

## Keyword  for each Department

- Based on frequency analysis, a word cloud visualization was prepared to identify the most frequently occurring keywords in the petition text content for each administrative department.
- This process allows us to observe the main topics being addressed by each department, providing insights into the key areas of focus and public concerns.

### 보건복지부

![1.png]("Keyword Department_image/1.png")


### 범무부

![2.jpeg]("Keyword Department_image/2-2.jpeg")

### 경찰청

![3.png]("Keyword Department_image//3.png")

### 고용노동부

![4.png]("Keyword Department_image/4.png")

### 행정안전부

![5.png]("Keyword Department_image//5.png")

### 행정안전부

![6.png]("Keyword Department_image/6.png")

### 교육부

![7.png]("Keyword Department_image/7.png")

### 인천광역시

![8.png]("Keyword Department_image//8.png")

### 서울특별시

![9.png]("Keyword Department_image/9.png")

### 환경부

![10.png]("Keyword Department_image/10.png")

we can check top keywords appears frequently for each department

---

## 3-gram analysis

- Significant keywords were identified for each department, with certain terms appearing frequently. However, keywords alone do not fully convey their context or meaning.
- For example, if "education" is the most frequent keyword, it is important to determine whether the public is advocating for specific types of education or opposing certain educational practices.
- Thus, further contextual analysis is needed to clarify the meaning behind each keyword, allowing us to better understand the public's demands.

![N-gram.png]("Keyword Department_image/N-gram.png")

---

## LDA Topic Modeling

- Topic modeling was conducted on the entire dataset to understand the overall interests of the public.
- The LDA technique was employed, and four methodologies were used to identify the optimal number of topics.
- Finally, the analysis was performed with five topics.
    
    ![LDA1.png]("Keyword Department_image/LDA1.png")
    

![LDA2.png]("Keyword Department_image/LDA2.png")

---

## Conclusion

- The results of the n-gram keyword analysis and LDA analysis revealed some differences, likely due to the highly specific nature of the petitions, which cover a wide range of detailed topics.
- As a result, fully understanding the concrete content of the petitions through LDA analysis alone proved to be challenging.
- However, the keyword analysis provided a clearer picture of the specific concerns raised in the petitions, highlighting issues that differ from widely discussed social trends, such as pedestrian protection at crosswalks, support for hikikomori (social withdrawal) individuals, and expanding parental leave.