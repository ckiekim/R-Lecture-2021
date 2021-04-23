# 4장 단원문제
# 1.
sum <- 0
for (i in 1:100) {
    if (i%%3==0 & i%%4!=0) {
        sum <- sum + i
    }
}
print(sum)

# 2.
multiple <- function(x, n) {
    sum <- 0
    for (i in 1:n) {
        if (i%%x == 0) {    # i가 x의 배수이면
            sum <- sum + i
        }
    }
    return(sum)
}
multiple(4, 19)

# 3
library(hflights)
str(hflights)
sum(is.na(hflights))

# 4
max_time <- max(hflights$AirTime, na.rm=T)
print(paste0(max_time%/%60,'시간 ',max_time%%60,'분'))

# 5
max(hflights$Distance, na.rm=T)

# 6
table(hflights$Cancelled)
table(hflights$Cancelled)[2]
