# 정규 표현식(Regular Expression)
library(stringr)

fruits <- c("1 apple,", "2 pears?", "3 bananas!!")
str_match(fruits, '[aeiou]')
str_match_all(fruits, '[aeiou]')

str_match(fruits, '\\d')
str_match(fruits, '[[:digit:]]')

str_match(fruits, '[[:punct:]]')
str_match_all(fruits, '[[:punct:]]')

gsub('[[:digit:]]', 'x', fruits)
gsub('[[:punct:]]', '_', fruits)

gsub('\\d', '', fruits)
gsub('\\s', '', fruits)
gsub('\\w', '', fruits)
