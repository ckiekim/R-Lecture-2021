# 10장 단원문제
library(caret)

##### 1. colon
library(survival)
clean_colon <- na.omit(colon)
clean_colon <- clean_colon[c(T, F),]
clean_colon$status <- factor(clean_colon$status)

formula <- status~rx+sex+age+obstruct+perfor+adhere+nodes+differ+extent+surg+node4

for (i in c(5,10,15,20)) {
    con <- trainControl(method='cv', number=i)
    rf <- train(formula, clean_colon, method='rf',
                metric='Accuracy', trControl=con)
    print(confusionMatrix(rf))
}

##### 2. ucla
ucla <- read.csv('https://stats.idre.ucla.edu/stat/data/binary.csv')
ucla$admit <- factor(ucla$admit)
control <- trainControl(method='cv', number=10)
formula <- admit ~ .

L <- train(formula, ucla, method='svmLinear',
           metric='Accuracy', trControl=control)
LW <- train(formula, ucla, method='svmLinearWeights',
            metric='Accuracy', trControl=control)
P <- train(formula, ucla, method='svmPoly',
           metric='Accuracy', trControl=control)
R <- train(formula, ucla, method='svmRadial',
           metric='Accuracy', trControl=control)
RW <- train(formula, ucla, method='svmRadialWeights',
            metric='Accuracy', trControl=control)
f100 <- train(formula, ucla, method='rf', ntree=100,
              metric='Accuracy', trControl=control)
f300 <- train(formula, ucla, method='rf', ntree=300,
              metric='Accuracy', trControl=control)
f500 <- train(formula, ucla, method='rf', ntree=500,
              metric='Accuracy', trControl=control)
r <- train(formula, ucla, method='rpart',
           metric='Accuracy', trControl=control)
k <- train(formula, ucla, method='knn',
           metric='Accuracy', trControl=control)
g <- train(formula, ucla, method='glm',
           metric='Accuracy', trControl=control)

resamp <- resamples(list(선형=L, 선형가중치=LW, 다항식=P, RBF=R,
                         가중치=RW, rf100=f100, rf300=f300, rf500=f500,
                         tree=r, knn=k, glm=g))
summary(resamp)
sort(resamp, decreasing=T)
dotplot(resamp)

##### 3. voice
voice <- read.csv('data/voice.csv')
head(voice)
control <- trainControl(method='cv', number=10)
formula <- label ~ .

L <- train(formula, voice, method='svmLinear',
           metric='Accuracy', trControl=control)
LW <- train(formula, voice, method='svmLinearWeights',
            metric='Accuracy', trControl=control)
P <- train(formula, voice, method='svmPoly',
           metric='Accuracy', trControl=control)
R <- train(formula, voice, method='svmRadial',
           metric='Accuracy', trControl=control)
RW <- train(formula, voice, method='svmRadialWeights',
            metric='Accuracy', trControl=control)
f100 <- train(formula, voice, method='rf', ntree=100,
              metric='Accuracy', trControl=control)
f300 <- train(formula, voice, method='rf', ntree=300,
              metric='Accuracy', trControl=control)
f500 <- train(formula, voice, method='rf', ntree=500,
              metric='Accuracy', trControl=control)
r <- train(formula, voice, method='rpart',
           metric='Accuracy', trControl=control)
k <- train(formula, voice, method='knn',
           metric='Accuracy', trControl=control)
g <- train(formula, voice, method='glm',
           metric='Accuracy', trControl=control)

resamp <- resamples(list(선형=L, 선형가중치=LW, 다항식=P, RBF=R,
                           가중치=RW, rf100=f100, rf300=f300, rf500=f500,
                           tree=r, knn=k, glm=g))
summary(resamp)
sort(resamp, decreasing=T)
dotplot(resamp)
