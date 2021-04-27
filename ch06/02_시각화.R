# 데이터 시각화
library(gapminder)
library(dplyr)

y <- gapminder %>%
    group_by(year, continent) %>%
    summarise(c_pop=sum(pop))
head(y, 10)

# 1단계 - 막 그래프
plot(y$year, y$c_pop)

# 2단계 - 마커의 색상을 대륙별로 다르게 지정
plot(y$year, y$c_pop, col=y$continent)

# 3단계 - 마커의 모양을 대륙별로 다르게 지정
plot(y$year, y$c_pop, col=y$continent, pch=c(1:5))
length(levels(y$continent))
plot(y$year, y$c_pop, col=y$continent, 
     pch=c(1:length(levels(y$continent))))

# 4단계 - 범례 표시
legend('topleft',
       legend=levels(y$continent),
       pch=c(1:length(levels(y$continent))),
       col=c(1:length(levels(y$continent))))

# 대륙별 인당 GDP, 기대수명
plot(gapminder$gdpPercap, gapminder$lifeExp,
     pch=c(1:length(levels(gapminder$continent))),
     col=gapminder$continent)
legend('bottomright',
       legend=levels(gapminder$continent),
       pch=c(1:length(levels(gapminder$continent))),
       col=c(1:length(levels(gapminder$continent))))

# 로그 스케일 적용
plot(log10(gapminder$gdpPercap), gapminder$lifeExp,
     pch=c(1:length(levels(gapminder$continent))),
     col=gapminder$continent)
legend('bottomright',
       legend=levels(gapminder$continent),
       pch=c(1:length(levels(gapminder$continent))),
       col=c(1:length(levels(gapminder$continent))))

# ggplot을 이용해서 그려보기
library(ggplot2)
ggplot(gapminder, aes(x=gdpPercap,y=lifeExp,col=continent)) +
    geom_point() +
    scale_x_log10()

# 인구수에 따라 포인트 크기 조절
ggplot(gapminder, aes(x=gdpPercap,y=lifeExp,
                      col=continent,size=pop)) +
    geom_point() +
    scale_x_log10()

# 포인트의 투명도 조절
ggplot(gapminder, aes(x=gdpPercap,y=lifeExp,
                      col=continent,size=pop)) +
    geom_point(alpha=0.5) +
    scale_x_log10()

# 2007년도 자료만 보기 - dplyr과 ggplot 연동
gapminder %>% 
    filter(year==2007) %>% 
    ggplot(aes(x=gdpPercap,y=lifeExp,
               col=continent,size=pop)) +
    geom_point(alpha=0.5) +
    scale_x_log10()

ggplot(gapminder, aes(x=gdpPercap,y=lifeExp,
                      col=continent,size=pop)) +
    geom_point(alpha=0.5) +
    scale_x_log10() +
    facet_wrap(~year)
