# R library를 이용한 전처리
library(RCurl)
library(XML)
library(stringr)

html <- readLines('https://en.wikipedia.org/wiki/Data_science')
html <- htmlParse(html, asText=T)
doc <- xpathSApply(html, '//p', xmlValue)

doc <- Corpus(VectorSource(doc))
doc <- tm_map(doc, content_transformer(tolower))  # 소문자 변환
doc <- tm_map(doc, removeNumbers)                 # 숫자 제거
doc <- tm_map(doc, removeWords, stopwords('en'))  # 불용어 제거
doc <- tm_map(doc, removePunctuation)             # 구둣점 제거
doc <- tm_map(doc, stripWhitespace)               # 앞뒤 공백 제거

dtm <- DocumentTermMatrix(doc)
dim(dtm)

m <- as.matrix(dtm)   # DTM list를 matrix로 변환
v <- sort(colSums(m), decreasing = T)
df <- data.frame(word=names(v), freq=v)
head(df)

library(wordcloud2)
d200 <- df[1:200,]
wordcloud2(d200)
