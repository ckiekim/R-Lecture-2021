# Wikipedia "data science"
library(RCurl)
library(XML)
library(stringr)

html <- readLines('https://en.wikipedia.org/wiki/Data_science')
html <- htmlParse(html, asText=T)
doc <- xpathSApply(html, '//p', xmlValue)
length(doc)
doc[1]
doc[2]
doc[3]
corpus <- doc[2:3]

# 모두 소문자로 변경
corpus <- tolower(corpus)
corpus[1]

# 숫자 제거
# 숫자를 표현하는 정규식: '\\d', '[[:digit:]]'
corpus <- gsub('\\d', '', corpus)
corpus[1]

# 구둣점 제거
corpus <- gsub('[[:punct:]]', '', corpus)
corpus[1]
# corpus <- gsub("[\\\\!\"#$%&'()*+,./:;<=>?@[\\^\\]_`{|}~-]",
#                '', corpus, perl=T)

# 끝에 있는 공백 제거
corpus <- gsub('\n', ' ', corpus)
corpus <- str_trim(corpus, side='right')

# 불용어 제거
stopwords <- c('a', 'the', 'and', 'in', 'of', 'to', 'but')
words <- str_split(corpus, ' ')  # 결과가 리스트로 나옴
unlist(words)   # 여러개의 리스트 엘리먼트를 하나의 벡터로

l <- list()     # 빈 리스트 생성
for (word in unlist(words)) {
    if (!word %in% stopwords) {
        l <- append(l, word)
    }
}
corpus <- paste(l, collapse = ' ')
corpus

`%notin%` <- Negate(`%in%`)      # ``, back quote
l <- list()     # 빈 리스트 생성
for (word in unlist(words)) {
    if (word %notin% stopwords) {
        l <- append(l, word)
    }
}
corpus <- paste(l, collapse = ' ')
corpus
