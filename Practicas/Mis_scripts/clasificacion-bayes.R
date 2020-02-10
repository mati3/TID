library(corpus)
library(maps)
library(e1071)
library(fdm2id)
library(tm)

procesatexto <- function(texto){
  Trans_review = texto
  Trans_review = as.matrix(Trans_review)
  #texto_split = strsplit(Trans_review, split=" ")
  
  # Eliminacion de paginas web
  #"http\\S*",
  # Elimina numeros
  Trans_review <- gsub("[[:digit:]]", "", Trans_review)
  # Elimina RT
  Trans_review <- gsub("RT","",Trans_review)
  # Elimina espacios en blanco múltiples
  Trans_review <- gsub("[\\s]+", "", Trans_review)
  # Elimina signos como @
  Trans_review <- gsub("([^[:alpha:] ]+|&amp;|&am;|\\n+)", "", Trans_review)
  # Eliminacion de signos de puntuación
  Trans_review <- gsub("[[:punct:]]", "", Trans_review)
  # Pasar de mayuscula a minuscula
  Trans_review <- tolower(Trans_review)
  return(as.data.frame(as.vector(Trans_review)))
}

algoritmo.naiveBayes <- function(texto_train,texto_test,label_train,label_test){
  # Calculamos los corpus de los comentarios pasados
  corpus_train = tm::VCorpus(tm::VectorSource(texto_train))
  corpus_test = tm::VCorpus(tm::VectorSource(texto_test))
  # Obtenemos la matriz de términos de esos comentarios
  dtm_train <- DocumentTermMatrix(corpus_train)
  dtm_test <- DocumentTermMatrix(corpus_test)
  # Obtenemos los términos más frecuentes
  # (Aquellos en los que se repiten en más de 5 textos)
  freq.words <- findFreqTerms(dtm_train, 5)
  # Recalculamos la matriz de términos en función de este concepto (Sparsity)
  dtm_freq_train <- DocumentTermMatrix(corpus_train, control=list(dictionary = freq.words))
  dtm_freq_test <- DocumentTermMatrix(corpus_test, control=list(dictionary = freq.words))
  #Función para convertir el peso de los términos en valores binarios:
  # yes=presente, no=ausente
  convert_counts <- function(x) {
    x <- ifelse(x > 0, "Yes", "No")
  }
  # Obtenemos matriz de términos con valores binarios en lugar de pesos (continuos)
  trainNB <- apply(dtm_freq_train, MARGIN = 2, convert_counts)
  testNB <- apply(dtm_freq_test, MARGIN = 2, convert_counts)
  # Entrenamos el clasificador con el conjunto de entrenamiento
  classifier <- naiveBayes(trainNB, as.factor(label_train), laplace = 1)
  # Hacemos las predicciones sobre el conjunto de test y calculamos los errores que tienen
  pred_test <- predict(classifier, newdata=testNB)
  pred_train <- predict(classifier, newdata=trainNB)
  Etest <- mean(pred_test!=label_test)
  Etrain <- mean(pred_train!=label_train)
  cat("-------------------------------\n")
  cat("**MATRIZ DE CONFUSIÓN TEST**\n")
  print(table(pred=pred_test,real=label_test))
  cat(paste("Error de Test: ",Etest*100," %\n\n"))
  cat("------------------------------------\n")
  cat("**MATRIZ DE CONFUSIÓN TRAIN**\n")
  print(table(pred=pred_train,real=label_train))
  cat(paste("Error de Train: ",Etrain*100," %\n"))
  cat("------------------------------------")
}

# Dividir dataset en datos entrenamiento y test, desde user_review

c=sample(2,nrow(user_reviews),replace=TRUE,prob=c(0.7,0.3))
user_train <- user_reviews[c==1,]
user_test <- user_reviews[c==2,]

#user_train$Translated_Review <- procesatexto(user_train$Translated_Review)
#user_test$Translated_Review <- procesatexto(user_test$Translated_Review)

algoritmo.naiveBayes(user_train$Translated_Review, user_test$Translated_Review, user_train$Sentiment, user_test$Sentiment)
