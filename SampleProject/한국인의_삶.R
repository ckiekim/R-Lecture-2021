library(foreign)
library(dplyr)
library(ggplot2)
library(readxl)

filename <- 'SampleProject/project_data/Koweps_hpc10_2015_beta1.sav'
raw_welfare <- read.spss(filename, to.data.frame=T)
welfare <- raw_welfare

View(welfare)
dim(welfare)
welfare <- rename(welfare,
                  sex=h10_g3,
                  birth=h10_g4,
                  marriage=h10_g10,
                  religion=h10_g11,
                  income=p1002_8aq1,
                  code_job=h10_eco9,
                  code_region=h10_reg7)

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

# 2. 나이와 월급의 관계계