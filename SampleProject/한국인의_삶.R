# 한국인의 삶을 분석하라
## 한국복지패널 데이터 활용
library(foreign)
library(dplyr)
library(ggplot2)
library(readxl)

# 데이터 분석 준비
filename <- 'SampleProject/project_data/Koweps_hpc10_2015_beta1.sav'
raw_welfare <- read.spss(filename, to.data.frame=T)
welfare <- raw_welfare

View(welfare)
dim(welfare)
welfare <- rename(welfare,
                  sex = h10_g3,           # 성별
                  birth = h10_g4,         # 태어난 연도
                  marriage = h10_g10,     # 혼인 상태
                  religion = h10_g11,     # 종교
                  income = p1002_8aq1,    # 월급
                  code_job = h10_eco9,    # 직종 코드
                  code_region = h10_reg7) # 지역 코드

# 1. 성별에 따른 월급 차이
## 성별 변수 검토 및 전처리
table(welfare$sex)          # 남자-1, 여자-2, 무응답-9
sum(is.na(welfare$sex))     # 결측치 확인
welfare$sex <- ifelse(welfare$sex == 1, 'male', 'female')
table(welfare$sex)
qplot(welfare$sex)

## 월급 변수 검토 및 전처리
summary(welfare$income)
qplot(welfare$income)
sum(is.na(welfare$income))  # 범위: 1~9998, 모름/무응답-9999
welfare$income <- ifelse(welfare$income %in% c(0, 9999), NA, welfare$income)

## 성별에 따른 월급 차이
sex_income <- welfare %>% 
    filter(!is.na(income)) %>% 
    group_by(sex) %>% 
    summarise(mean_income=mean(income))
sex_income
ggplot(sex_income, aes(x=sex, y=mean_income, fill=sex)) +
    geom_col()

# 2. 나이와 월급의 관계
## 나이 변수 검토 및 전처리
class(welfare$birth)
summary(welfare$birth)
qplot(welfare$birth)
sum(is.na(welfare$birth))

## 파생변수 나이 만들기
welfare$age <- 2015 - welfare$birth + 1
summary(welfare$age)
qplot(welfare$age)

## 나이에 따른 월급 평균표 만들기
age_income <- welfare %>% 
    filter(!is.na(income)) %>% 
    group_by(age) %>% 
    summarise(mean_income = mean(income))
ggplot(age_income, aes(x=age, y=mean_income)) +
    geom_line()

# 3. 연령대에 따른 월급 차이
## 파생변수 연령대 만들기
welfare <- welfare %>% 
    mutate(ageg = ifelse(age < 30, 'young',
                         ifelse(age < 60, 'middle', 'old')))
table(welfare$ageg)
qplot(welfare$ageg)

## 연령대별 월급 평균표 만들어 비교
ageg_income <- welfare %>% 
    filter(!is.na(income)) %>% 
    group_by(ageg) %>% 
    summarise(mean_income = mean(income))
ggplot(ageg_income, aes(x=ageg, y=mean_income)) +
    geom_col()
ggplot(ageg_income, aes(x=ageg, y=mean_income, fill=ageg)) +
    geom_col() +
    scale_x_discrete(limits = c("young", "middle", "old"))

# 4. 연령대 및 성별 월급 차이
## 연령대 및 성별 월급 평균표 만들기
sex_income <- welfare %>%
    filter(!is.na(income)) %>%
    group_by(ageg, sex) %>%
    summarise(mean_income = mean(income))
sex_income

## 그래프 만들어 비교하기
ggplot(sex_income, aes(x=ageg, y=mean_income, fill=sex)) +
    geom_col() +
    scale_x_discrete(limits = c("young", "middle", "old"))
ggplot(sex_income, aes(x=ageg, y=mean_income, fill=sex)) +
    geom_col(position='dodge') +
    scale_x_discrete(limits = c("young", "middle", "old"))

## 나이 및 성별 월급 차이
sex_age <- welfare %>%
    filter(!is.na(income)) %>%
    group_by(age, sex) %>%
    summarise(mean_income = mean(income))
ggplot(sex_age, aes(x=age, y=mean_income, col=sex)) + 
    geom_line()

# 5. 직업별 월급 차이
unique(welfare$code_job)
list_job <- read_excel("SampleProject/project_data/Koweps_Codebook.xlsx", 
                       col_names = T, sheet = 2)
head(list_job)

## welfare 와 직업명 join
welfare <- left_join(welfare, list_job, id='code_job')
welfare %>%
    filter(!is.na(code_job)) %>%
    select(code_job, job) %>%
    head()

## 직업별 월급 평균표
job_income <- welfare %>%
    filter(!is.na(job) & !is.na(income)) %>%
    group_by(job) %>%
    summarise(mean_income = mean(income))

## 상위 10개
top10 <- job_income %>%
    arrange(desc(mean_income)) %>%
    head(10)
ggplot(top10, aes(x=reorder(job, mean_income), y=mean_income)) +
    geom_col() +
    coord_flip()

## 하위 10개
bottom10 <- job_income %>%
    arrange(mean_income) %>%
    head(10)
ggplot(bottom10, aes(x=reorder(job, -mean_income), y=mean_income)) +
    geom_col() +
    coord_flip() +
    ylim(0, 850)

# 6. 성별 직업 빈도
## 남성 직업 빈도 상위 10개
job_male <- welfare %>%
    filter(!is.na(job) & sex == "male") %>%
    group_by(job) %>%
    summarise(n = n()) %>%
    arrange(desc(n)) %>%
    head(10)
job_male
ggplot(job_male, aes(x=reorder(job, n), y=n)) +
    geom_col() +
    coord_flip()

## 여성 직업 빈도 상위 10 개 추출
job_female <- welfare %>%
    filter(!is.na(job) & sex == "female") %>%
    group_by(job) %>%
    summarise(n = n()) %>%
    arrange(desc(n)) %>%
    head(10)
job_female
ggplot(job_female, aes(x=reorder(job, n), y=n)) +
    geom_col() +
    coord_flip()

# 7. 종교 유무에 따른 이혼율
## 종교 변수 검토 및 전처리
class(welfare$religion)
table(welfare$religion)
welfare$religion <- ifelse(welfare$religion == 1, "yes", "no")
qplot(welfare$religion)

## 혼인 상태 변수 검토 및 전처리
class(welfare$marriage)
table(welfare$marriage)

## 이혼 여부 변수 만들기
welfare$group_marriage <- ifelse(welfare$marriage == 1, "marriage",
                                 ifelse(welfare$marriage == 3, "divorce", NA))
table(welfare$group_marriage)
qplot(welfare$group_marriage)

## 종교 유무에 따른 이혼율 표 만들기
religion_marriage <- welfare %>%
    filter(!is.na(group_marriage)) %>%
    group_by(religion, group_marriage) %>%
    summarise(n = n()) %>%
    mutate(tot_group = sum(n)) %>%
    mutate(pct = round(n/tot_group*100, 1))
religion_marriage
### count() 활용
religion_marriage <- welfare %>%
    filter(!is.na(group_marriage)) %>%
    count(religion, group_marriage) %>%
    group_by(religion) %>%
    mutate(pct = round(n/sum(n)*100, 1))
religion_marriage

## 이혼 추출
divorce <- religion_marriage %>%
    filter(group_marriage == "divorce") %>%
    select(religion, pct)
divorce
ggplot(divorce, aes(x=religion, y=pct, fill=religion)) + 
    geom_col()

## 연령대, 종교유무, 결혼상태별 비율표 만들기
ageg_religion_marriage <- welfare %>%
    filter(!is.na(group_marriage) & ageg != "young") %>%
    group_by(ageg, religion, group_marriage) %>%
    summarise(n = n()) %>%
    mutate(tot_group = sum(n)) %>%
    mutate(pct = round(n/tot_group*100, 1))
ageg_religion_marriage
### count() 활용
ageg_religion_marriage <- welfare %>%
    filter(!is.na(group_marriage) & ageg != "young") %>%
    count(ageg, religion, group_marriage) %>%
    group_by(ageg, religion) %>%
    mutate(pct = round(n/sum(n)*100, 1))
ageg_religion_marriage

## 연령대 및 종교 유무별 이혼율 표 만들기
df_divorce <- ageg_religion_marriage %>%
    filter(group_marriage == "divorce") %>%
    select(ageg, religion, pct)
df_divorce

ggplot(df_divorce, aes(x=ageg, y=pct, fill=religion )) +
    geom_col(position="dodge")
