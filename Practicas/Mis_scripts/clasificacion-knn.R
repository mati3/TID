library("class")

set.seed(3)

c=sample(2,nrow(user_reviews),replace=TRUE,prob=c(0.7,0.3))
data_train <- user_reviews[c==1,]
data_test <- user_reviews[c==2,]

colnames(data_train)<-c("text", "label")
colnames(data_test)<-c("text", "label")

data_train$label <- as.numeric(data_train$label)
data_train$text <- as.numeric(data_train$text)
data_test$label <- as.numeric(data_test$label)
data_test$text <- as.numeric(data_test$text)

trainClass<-data_train[,"label"]
trueClass<-data_test[,"label"]

knnClass <- knn (data_train, data_test, trainClass)
# Matriz de confusión:
nnTabla <- table ("1-NN" = knnClass, Reuters = trueClass); nnTabla
sum(diag(nnTabla))/nrow(data_test)

