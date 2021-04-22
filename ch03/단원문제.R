# 3장 단원문제
# 1
x <- seq(3, 100, by=3)
x
y <- seq(4, 100, by=4)
y

# 2
sum(intersect(x, y))

# 3
? airquality
# New York City

# 4
# numeric	Temperature (degrees F) - Fahrenheit

# 5
max_wind<-max(airquality$Wind)
wind.day<-airquality[airquality$Wind==max_wind, 5:6]
wind.day
date<-paste0('1973-',wind.day$Month,'-',wind.day$Day)
date               # 문자열
date_type <- as.Date(date)  # Date
date_type

# 6
sum(is.na(airquality))

# 7
? quakes
# Fiji

# 8
max(quakes$mag)
quakes[quakes$mag==max(quakes$mag),]

# Date type으로 
days <- seq(as.Date('2021-04-01'), as.Date('2021-04-30'), by=1)
days
