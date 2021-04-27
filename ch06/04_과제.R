# 데이터 시각화 과제
library(gapminder)
library(dplyr)
library(ggplot2)

# 1-1
x <- gapminder %>% 
    filter(year==1952) %>% 
    select(country, pop) %>% 
    arrange(desc(pop)) %>% 
    head()
pie(x$pop, x$country)
barplot(x$pop, names.arg=x$country)

# 1-2
for (i in seq(1952,2007,5)) {
    x <- gapminder %>% 
        filter(year==i) %>% 
        select(country, pop) %>% 
        arrange(desc(pop)) %>% 
        head()
    pie(as.numeric(x$pop), x$country)
    barplot(x$pop, names.arg=x$country)
    title(i)
}

# 2
library(tidyr)

# 2-1 airquality
air_tidy <- gather(airquality, key='Measure', value='Value',
                   -Day, -Month)
head(air_tidy)
tail(air_tidy)

air_tidy %>% 
    ggplot(aes(Day, Value, col=Measure)) +
    geom_point() +
    facet_wrap(~Month)

# 2-2 iris
iris_tidy <- gather(iris, key='feat', value='Value',
                    -Species)
iris_tidy %>% 
    ggplot(aes(feat, Value, col=Species)) +
    geom_point(position='jitter')

# 3
library(gridExtra)
seto <- filter(iris, Species=='setosa')
vers <- filter(iris, Species=='versicolor')
virg <- filter(iris, Species=='virginica')

seto_s <- seto %>% 
    ggplot(aes(Sepal.Length,Sepal.Width,col=Species)) +
    geom_point()
seto_p <- seto %>% 
    ggplot(aes(Petal.Length,Petal.Width,col=Species)) +
    geom_point()
vers_s <- vers %>% 
    ggplot(aes(Sepal.Length,Sepal.Width,col=Species)) +
    geom_point()
vers_p <- vers %>% 
    ggplot(aes(Petal.Length,Petal.Width,col=Species)) +
    geom_point()
virg_s <- virg %>% 
    ggplot(aes(Sepal.Length,Sepal.Width,col=Species)) +
    geom_point()
virg_p <- virg %>% 
    ggplot(aes(Petal.Length,Petal.Width,col=Species)) +
    geom_point()

grid.arrange(seto_s,seto_p,vers_s,vers_p,
             virg_s,virg_p,ncol=2)

# 3-2
seto_mean <- apply(iris[iris$Species=='setosa',1:4],2,mean)
vers_mean <- apply(iris[iris$Species=='versicolor',1:4],2,mean)
virg_mean <- apply(iris[iris$Species=='virginica',1:4],2,mean)
mean_of_iris <- rbind(seto_mean, vers_mean, virg_mean)

df <- iris %>% 
    group_by(Species) %>% 
    summarise(sepal_length=mean(Sepal.Length), sepal_width=mean(Sepal.Width),
              petal_length=mean(Petal.Length), petal_width=mean(Petal.Width))
df

barplot(as.matrix(df[,2:5]), beside=T,
        main='품종별 평균', ylim=c(0,8), col=c('red','green','blue'))
legend('topright',
       legend=c('Setosa','Versicolor','Virginica'),
       fill=c('red','green','blue'))

# 3-3
par(mfrow=c(3,1))
boxplot(seto$Sepal.Length, seto$Sepal.Width,
        seto$Petal.Length, seto$Petal.Width,
        col=c('red','yellow','green','blue'),
        names=c('Sepal.Length','Sepal.Width','Petal.Length','Petal.Width'),
        main='Setosa')
boxplot(vers$Sepal.Length, vers$Sepal.Width,
        vers$Petal.Length, vers$Petal.Width,
        col=c('red','yellow','green','blue'),
        names=c('Sepal.Length','Sepal.Width','Petal.Length','Petal.Width'),
        main='Versicolor')
boxplot(virg$Sepal.Length, virg$Sepal.Width,
        virg$Petal.Length, virg$Petal.Width,
        col=c('red','yellow','green','blue'),
        names=c('Sepal.Length','Sepal.Width','Petal.Length','Petal.Width'),
        main='Virginica')
par(mfrow=c(1,1))