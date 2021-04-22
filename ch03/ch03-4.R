# 데이터 프레임
name <- c('철수', '춘향', '길동')
age <- c(22, 20, 25)
gender <- factor(c('M', 'F', 'M'))
blood_type <- factor(c('A', 'O', 'B'))

patients<-data.frame(name, age, gender, blood_type)
patients

patients$name
typeof(patients$name)
patients[1,]     # 첫번째 행 모두
patients[,2]     # patients$age 와 동일
patients[2,3]    # F
patients$gender[3]
patients[patients$name=='철수',]   # patients[1,], filtering
patients[patients$name=='철수',c('age','gender')]  # selection

# 데이터프레임의 속성명을 변수명으로 사용(attach ~ detach)
head(cars)
attach(cars)
speed
dist
detach(cars)
speed        # 에러: 객체 'speed'를 찾을 수 없습니다

mean(cars$speed)
max(cars$dist)
with(cars, mean(speed))

# subset
subset(cars, speed>20)
cars[cars$speed>20,]
subset(cars, speed>20, select=c(dist))
subset(cars, speed>20, select=-c(dist))

# 결측값(NA) 처리
head(airquality)
str(airquality)
sum(airquality$Ozone)

head(na.omit(airquality))  # NA가 포함된 행 전체를 제거

# 병합(merge)
patients
patients1 <- data.frame(name, age, gender)
patients2 <- data.frame(name, blood_type)
merge(patients1, patients2, by='name')
