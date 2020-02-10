library("tm")
library(SparseM)
library(RTextTools)

set.seed(1)

c=sample(2,nrow(user_reviews),replace=TRUE,prob=c(0.7,0.3))
user_reviews2 <- user_reviews[2:3]
# si hay columna nan la eliminamos
user_reviews2$Sentiment <- factor(user_reviews2$Sentiment, levels = c("Negative","Neutral","Positive"))
data_train <- user_reviews2[c==1,]
data_test <- user_reviews2[c==2,]
data_train <- na.omit(data_train)
data_test <- na.omit(data_test)
colnames(data_train)<-c("text", "label")
colnames(data_test)<-c("text", "label")

corpus = Corpus(VectorSource(data_train$text))
tdm <- tm::DocumentTermMatrix(corpus, list(bounds= list(global= c(5,Inf))))
tdm.tfidf <- suppressWarnings(tm::weightTfIdf(tdm))

training_codes = data_train$label

container <- create_container(tdm,               # creates a 'container' obj for training, classifying, and analyzing docs
                              t(training_codes), # labels or the Y variable / outcome we want to train on
                              trainSize = 1:2999, # dim(data_train)
                              virgin = FALSE)

models <- train_models(container,            
                       algorithms=c("SVM"))

results <- classify_models(container, models)

# building a confusion matrix to see accuracy of prediction results
out = data.frame(model_sentiment = results$SVM_LABEL,    
                 model_prob = results$SVM_PROB,
                 actual_party = data_train$label[1:2999]) 

(z = as.matrix(table(out[,1], out[,3])))   # display the confusion matrix.

(pct = round(((z[1,1] + z[2,2] + z[3,3] )/sum(z))*100, 2))      # prediction accuracy in % terms


#test
text = data_test$text 

cor = Corpus(VectorSource(text)) # crea otra vez el corpus 
dtm <- DocumentTermMatrix(cor, list(bounds= list(global= c(5,Inf))))
dtm.test <- suppressWarnings(weightTfIdf(dtm))
row.names(dtm.test) = (nrow(dtm)+1):(nrow(dtm)+nrow(dtm.test))     # row naming for doc ID
dtm.f = c(tdm, dtm.test)    # concatenating the dtms
training_codes.f = c(training_codes, 
                     rep(NA, length(data_test))) 

container.f = create_container(dtm.f,      # El container de la misma manera q antes
                               t(training_codes.f), trainSize=1:nrow(tdm), 
                               testSize = 1:1291, virgin = T)
#######
model.f = train_models(container.f, algorithms = c("SVM")) 

predicted <- classify_models(container.f, model.f) 

out = data.frame(model_sentiment = predicted$SVM_LABEL,    # again, building a confusion matrix
                 model_prob = predicted$SVM_PROB,
                 text = data_test$label)

(z = as.matrix(table(out[,1], out[,3]))) # matriz de confusion
(pct = round(((z[1,1] + z[2,2] + z[3,3] )/sum(z))*100, 2)) 
