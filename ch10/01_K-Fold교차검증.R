# K-Fold 교차 검증(Cross Validation)
library(caret)

# iris 데이터를 섞는다.
set.seed(2021)
data <- iris[sample(nrow(iris)),]
head(data)

# K-Fold CV, K=5
k <- 5
q <- nrow(data) / k
l <- 1:nrow(data)
acc <- 0

for (i in 1:k) {
    test_list <- ((i-1)*q+1) : (i*q)
    data_test <- data[test_list,]
    data_train <- data[-test_list,]
    rf <- train(Species~., data_train, method='rf')
    pred <- predict(rf, data_test)
    t <- table(pred, data_test$Species)
    print((t[1,1]+t[2,2]+t[3,3])/nrow(data_test))
    acc <- acc + (t[1,1]+t[2,2]+t[3,3])/nrow(data_test)
}

average_accuracy <- acc / k
average_accuracy

################################
# caret 라이브러리를 이용한 코드
################################
control <- trainControl(method='cv', number=5)
new_rf <- train(Species~., data=iris, method='rf',
                metric='Accuracy', trControl=control)
confusionMatrix(new_rf)    

###############################
# 4개 모델에 적용
###############################
control <- trainControl(method='cv', number=5)
dt <- train(Species~., data=iris, method='rpart',
            metric='Accuracy', trControl=control)
rf <- train(Species~., data=iris, method='rf',
            metric='Accuracy', trControl=control)
sv <- train(Species~., data=iris, method='svmRadial',
            metric='Accuracy', trControl=control)
kn <- train(Species~., data=iris, method='knn',
            metric='Accuracy', trControl=control)

resamp <- resamples(list(결정트리=dt, 랜덤포레스트=rf,
                         SVM=sv, KNN=kn))
summary(resamp)
sort(resamp, decreasing=T)
