# Random Forest
library(caret)
library(randomForest)

set.seed(2021)
train_index <- createDataPartition(iris$Species,p=0.8,list=F)
iris_train <- iris[train_index,]
iris_test <- iris[-train_index,]

# 모델링/학습
rf <- randomForest(Species ~ ., iris_train)
rf

# 예측
pred <- predict(rf, iris_test, type='class')

# 평가
confusionMatrix(pred, iris_test$Species)

# 시각화
plot(rf)

# 하이퍼 파라메터
small_forest <- randomForest(Species ~ ., iris_train,
                             ntree=100, nodesize=4)
s_pred <- predict(small_forest, iris_test, type='class')
confusionMatrix(s_pred, iris_test$Species)
plot(small_forest)
