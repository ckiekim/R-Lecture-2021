# R library를 이용한 전처리
library(RCurl)
library(XML)
library(stringr)

html <- readLines('https://en.wikipedia.org/wiki/Data_science')
html <- htmlParse(html, asText=T)
doc <- xpathSApply(html, '//p', xmlValue)
