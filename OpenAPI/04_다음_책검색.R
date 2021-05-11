# 다음 책 검색 
library(httr)
library(jsonlite)

kakao_api_key <- readLines('OpenAPI/kakao_api_key.txt', encoding='UTF-8')
kakao_api_key

base_url <- 'https://dapi.kakao.com/v3/search/book'
keyword <- URLencode(iconv('데이터 분석', to='UTF-8'))
keyword
query <- paste0('target=title&query=', keyword)
url <- paste(base_url, query, sep='?')
url

auth_key <- paste('KakaoAK', kakao_api_key)
auth_key
res <- GET(url, add_headers('Authorization'=auth_key))
res
result <- fromJSON(as.character(res))
class(result)          # 결과는 리스트
df <- as.data.frame(result)
View(df)

write.csv(df, 'OpenAPI/book.csv', fileEncoding='utf-8')
# 결과가 리스트이기 때문에 매트릭스로 변환하여 저장
write.csv(as.matrix(df), 'OpenAPI/book.csv', 
          fileEncoding='utf-8', row.names=F)
# 내용중에 ','가 있어서 제대로 못읽어서 sep를 \t로 변경
write.table(as.matrix(df), 'OpenAPI/book.tsv', 
            fileEncoding='utf-8', row.names=F, sep='\t')

df2 <- read.csv('OpenAPI/book.tsv', fileEncoding='utf-8', sep='\t')
View(df2)
