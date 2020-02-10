library(tree)
library(party)
library(tm)
library("rattle")
library("rpart")
library("rpart.plot")


# otra prueba, descompongo en dos documentos, prueba ok
ind=sample(2,nrow(dtm),replace=TRUE,prob=c(0.7,0.3))
tfidf_DT <- suppressWarnings(weightTfIdf(dtm))
terms_DT <- tfidf_DT$dimnames$Terms
dtidf_entrena <- as.vector(tfidf_DT[ , ind==1])
names(dtidf_entrena) <- terms_DT
dtidf_test <- as.vector(tfidf_DT[ , ind==2])
names(dtidf_test) <- terms_DT

tf_entrena <- as.matrix(dtidf_entrena)
tf_test <- as.matrix(dtidf_test)
# comprobamos las dimensiones de ambas matrices, deben ser iguales
all(colnames(dtidf_entrena) == colnames(dtidf_test)) 
all(colnames(tf_entrena) == colnames(tf_test)) 
# hasta aqui ok yujuuuu
################################


c=sample(2,nrow(Trans_review),replace=TRUE,prob=c(0.7,0.3))
user_train <- Trans_review[c==1,]
user_test <- Trans_review[c==2,]
                         
#############################3

rpart <- rpart(Sentiment ~ Rating, data=user_train,
               method="class",
               parms=list(split="information"),
               control=rpart.control(minsplit=30,
                                     minbucket=10,
                                     cp=0.0041,
                                     usesurrogate=0,
                                     maxsurrogate=0))
fancyRpartPlot(rpart)
plotcp(rpart)
printcp(rpart)
rpart <- rpart(Sentiment ~ Category, data=user_train,
               method="class",
               parms=list(split="information"),
               control=rpart.control(minsplit=30,
                                     minbucket=10,
                                     cp=0.00,
                                     usesurrogate=0,
                                     maxsurrogate=0))
fancyRpartPlot(rpart)

fancyRpartPlot(rpart)
rpart <- rpart(Sentiment ~ Installs, data=user_train,
      method="class",
      parms=list(split="information"),
      control=rpart.control(minsplit=30,
      minbucket=10,
      cp=0.00,
      usesurrogate=0,
      maxsurrogate=0))
fancyRpartPlot(rpart)

###########################################3
corpus = Corpus(VectorSource(datos_total$Translated_Review))
# Creamos matriz de términos con las palabras de los comentarios. Entonces cada fila, va a estar formada
tdm <- tm::DocumentTermMatrix(corpus)
tdm.tfidf <- tm::weightTfIdf(tdm)
reviews = as.data.frame(cbind(datos_total$Sentiment, as.matrix(tdm.tfidf)))

reviews <- na.omit(reviews)

preparado_train <- reviews[1:1500,]
preparado_test <- reviews[-(1:1500),]

reviews_tree = rpart(V1~., method = "class", data= preparado_train)
prp(reviews_tree)
