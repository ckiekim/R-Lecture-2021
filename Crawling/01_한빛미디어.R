# 한빛미디어 사이트로 웹 크롤링 연습하기
library(rvest)
library(stringr)
library(dplyr)

# 웹 사이트 읽기
base_url <- 'https://www.hanbit.co.kr/media/books'
sub_url <- 'new_book_list.html'
url <- paste(base_url, sub_url, sep='/')
url
html <- read_html(url)

container <- html_node(html, '#container')  # id="container"
book_list <- html_node(container, '.new_book_list_wrap')  # class="..."
sub_book_list <- html_node(book_list, '.sub_book_list_area')
lis <- html_nodes(sub_book_list, 'li')      # <li> 모두 찾기
li <- lis[1]
info <- html_node(li, '.info')
title <- html_node(info, '.book_tit')
title <- html_text(title)
writer <- info %>% 
    html_node('.book_writer') %>% 
    html_text()
writer

title_vector <- c()
writer_vector <- c()
for (li in lis) {
    info <- html_node(li, '.info')
    title <- info %>% 
        html_node('.book_tit') %>% 
        html_text()
    writer <- info %>% 
        html_node('.book_writer') %>% 
        html_text()
    title_vector <- c(title_vector, title)
    writer_vector <- c(writer_vector, writer)
}
new_books <- data.frame(
    title=title_vector,
    writer=writer_vector
)
View(new_books)

#########################
# 도서 세부내용 크롤링
#########################
li <- lis[1]
href <- li %>% 
    html_node('.info') %>% 
    html_node('a') %>% 
    html_attr('href')
href
book_url <- paste(base_url, href, sep='/')
book_url
book_html <- read_html(book_url)
info_list <- html_node(book_html, 'ul.info_list')
lis <- html_nodes(info_list, 'li')
page_li <- lis[4]
page <- html_node(page_li, 'span') %>% html_text()    
len <- nchar(page) 
len
page <- as.integer(substring(page, 1, len-2))
page




