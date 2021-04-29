# 단순 선형회귀의 적용
# Cars 데이터
str(cars)
plot(cars)
car_model <- lm(dist~speed, data=cars)
coef(car_model)
# 회귀식: dist = 3.9324 * speed - 17.5791
abline(car_model, col='red')

summary(car_model)
par(mfrow=c(2,2))
plot(car_model)  # 그릴 공간이 크면 작동
# plot(car_model,1)
# plot(car_model,2)
# plot(car_model,3)
# plot(car_model,4)
par(mfrow=c(1,1))

# 속도 21.5, 제동거리는?
nx1 <- data.frame(speed=c(21.5))
predict(car_model, nx1)

# 고차식(polynomial) 적용하면 어떻게 될까?
lm2 <- lm(dist~poly(speed,2), data=cars)
plot(cars)
x <- seq(4, 25, length.out=211)
head(x)
y <- predict(lm2, data.frame(speed=x))
lines(x, y, col='purple', lwd=2)
abline(car_model, col='red', lwd=2)

summary(lm2)

# cars 1차식부터 4차식까지
x <- seq(4, 25, length.out=211)
colors <- c('red','purple','darkorange','blue')
plot(cars)
for (i in 1:4) {
    m <- lm(dist~poly(speed, i), data=cars)
    assign(paste('m',i,sep='.'), m)
    y <- predict(m, data.frame(speed=x))
    lines(x, y, col=colors[i], lwd=2)
}

# 분산 분석(anova)
anova(m.1, m.2, m.3, m.4)

# Women data
women
plot(women)
m <- lm(weight~height, data=women)
abline(m, col='red', lwd=2)
summary(m)

# 2차식으로 모델링
m2 <- lm(weight ~ poly(height, 2), data=women)
x <- seq(58, 72, length.out=300)
y <- predict(m2, data.frame(height=x))
lines(x, y, col='blue', lwd=2)
summary(m2)
coef(m2)
