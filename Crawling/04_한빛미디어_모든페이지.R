# 한빛미디어 사이트 - 책 모든 페이지지 웹 크롤링 
library(rvest)
library(stringr)
library(dplyr)

title_vector <- c()
writer_vector <- c()
page_vector <- c()
price_vector <- c()

base_url <- 'https://www.hanbit.co.kr/media/books'
sub_url <- '/new_book_list.html'
for (page in c(1:25)) {
    print(page)
    query <- sprintf('?page=%d&cate_cd=&srt=&searchKey=&keyWord=', page)
    url <- paste0(base_url, sub_url, query)
    html <- read_html(url)
    
    lis <- html %>% 
        html_node('#container') %>% 
        html_node('.new_book_list_wrap') %>% 
        html_node('.sub_book_list_area') %>% 
        html_nodes('li') 
    
    for (li in lis) {
        info <- html_node(li, '.info')
        title <- info %>% 
            html_node('.book_tit') %>% 
            html_text()
        writer <- info %>% 
            html_node('.book_writer') %>% 
            html_text()
        href <- li %>% 
            html_node('.info') %>% 
            html_node('a') %>% 
            html_attr('href')
        book_url <- paste(base_url, href, sep='/')
        book_html <- read_html(book_url)
        info_list <- html_node(book_html, 'ul.info_list')
        book_lis <- html_nodes(info_list, 'li')
        for (book_li in book_lis) {
            item <- book_li %>% 
                html_node('strong') %>% 
                html_text()
            if (substring(item, 1, 3) == '페이지') {
                page <- book_li %>% 
                    html_node('span') %>% 
                    html_text()
                len <- str_length(page) 
                page <- as.integer(substring(page, 1, len-2))
                break
            }
        }
        
        pay_info <- html_node(book_html, '.payment_box.curr')
        ps <- html_nodes(pay_info, 'p')
        # 12페이지 두개의 책에서 price정보를 얻을 수 없음
        if (length(ps) == 0) {
            price <- 0
        } else {
            price <- ps[2] %>% 
                html_node('.pbr') %>% 
                html_node('strong') %>% 
                html_text()
            price <- as.integer(gsub(',','',price))
        }
        print(paste(title, price, sep=', '))
        
        title_vector <- c(title_vector, title)
        writer_vector <- c(writer_vector, writer)
        page_vector <- c(page_vector, page)
        price_vector <- c(price_vector, price)
    }
}

new_books <- data.frame(
    title=title_vector,
    writer=writer_vector,
    page=page_vector,
    price=price_vector
)
View(new_books)

# 파일에 저장하기
write.csv(new_books, 'data/한빛도서.csv', 
          fileEncoding='utf-8', row.names=F)
df <- read.csv('data/한빛도서.csv', fileEncoding='utf-8')
View(df)
