# 문자열 처리
# [R] R에서 String(문자열) 처리하기 - 베어베어스 참조
library(stringr)

# 1. Character로 형 변환
example <- 1
typeof(example)   # double (숫자)
example <- as.character(example)
typeof(example)   # character

# 입력을 받는 경우
input <- readline('Prompt> ')
input
i <- as.numeric(input)
3 * i

# 2. String 이어 붙이기
paste('A','quick','brown','fox')  # "A quick brown fox"
paste0('A','quick','brown','fox') # "Aquickbrownfox"
paste('A','quick','brown','fox',sep='-')
s <- paste('A','quick','brown','fox',sep='-')
sample <- c('A','quick','brown','fox')
paste(sample)
paste(sample, collapse='-')
str_c(sample, '1', sep='_')
str_c(sample, '1', sep='_', collapse='@@')

# 3. Character 개수 카운트
x <- 'Hello'
nchar(x)
h <- '안녕하세요'
nchar(h)
str_length(h)

# 4. 소문자 변환
tolower(x)

# 5. 대문자 변환
toupper(x)

# 6. 2개의 character vector를 중복되는 항목 없이 합하기
str_1 <- c("hello", "world", "r", "program")
str_2 <- c("hi", "world", "r", "coding")
union(str_1, str_2)       # 합집합

# 7. 2개의 character vector에서 공통된 항목 추출 
intersect(str_1, str_2)   # 교집합

# 8. 차집합
setdiff(str_1, str_2)

# 9. 2개의 character vector 동일 여부 확인 (순서에 관계없이)
str_3 <- c("r", "hello", "program", "world")
setequal(str_1, str_2)
setequal(str_1, str_3)

# 10. 공백 없애기
vector_1 <- c("   Hello World!  ", "    Hi R!    ")
str_trim(vector_1, side='left')
str_trim(vector_1, side='right')
str_trim(vector_1, side='both')

# 11. String 반복
str_dup(x, 3)
rep(x, 3)

# 12. Substring(String의 일정 부분) 추출
string_1 <- "Hello World"
substr(string_1, 7, 9)
substring(string_1, 7, 9)
str_sub(string_1, 7, 9)
substr(string_1, 7)       # error
substring(string_1, 7)    # 7번째부터 끝까지
str_sub(string_1, 7)
str_sub(string_1, 7, -1)
str_sub(string_1, 7, -3)
string_1[7:9]             # NA NA NA

# 13. String의 특정 위치에 있는 값 바꾸기
string_1 <- "Today is Monday"
substr(string_1, 10, 12) <- "Sun"
string_1
substr(string_1, 10, 12) <- "Thurs"
string_1        # "Today is Thuday"

# 14. 특정 패턴(문자열)을 기준으로 String 자르기
strsplit(string_1, split=' ')
str_split(string_1, pattern=' ')
str_split(string_1, pattern=' ', n=2)
str_split(string_1, pattern=' ', simplify = T)  # matrix
s <- str_split(string_1, pattern=' ')
typeof(s)
s[[1]]
s[[1]][1]
# 리스트를 벡터로 변환 : unlist()
unlist(s)
paste(unlist(s), collapse = ' ')

# 15. 특정 패턴(문자열) 찾기 (기본 function)
vector_1 <- c("Xman", "Superman", "Joker")
grep('man', vector_1)
grepl('man', vector_1)
regexpr("man", vector_1)
gregexpr("man", vector_1)

# 16. 특정 패턴(문자열) 찾기 (stringr package function)
fruit <- c("apple", "banana", "cherry")
str_count(fruit, 'a')
str_detect(fruit, 'a')
str_locate(fruit, 'a')
str_locate_all(fruit, 'a')

people <- c("rorori", "emilia", "youna")
str_match(people, 'o(\\D)') # \\D는 non-digit character를 의미

# 17. 특정 패턴(문자열) 찾아서 다른 패턴(문자열)으로 바꾸기
fruits <- c("one apple", "two pears", "three bananas")
sub('a', 'A', fruits)
gsub('a', 'A', fruits)
str_replace(fruits, 'a', 'A')
str_replace_all(fruits, 'a', 'A')
