library("dplyr")
library("tidytext")
library("corpus")
library(stringr)
library(purrr)
library(tm)


#Trans_review = datospro$Translated_Review
Trans_review = datospro
#Trans_review = user_reviews$Translated_Review
Trans_review$Translated_Review = as.matrix(Trans_review$Translated_Review)
#texto_split = strsplit(Trans_review, split=" ")

# Eliminacion de paginas web
#"http\\S*",
# Elimina numeros
Trans_review$Translated_Review <- gsub("[[:digit:]]", "", Trans_review$Translated_Review)
# Elimina RT
Trans_review$Translated_Review <- gsub("RT","",Trans_review$Translated_Review)
# Elimina espacios en blanco múltiples
Trans_review$Translated_Review <- gsub("[\\s]+", "", Trans_review$Translated_Review)
# Elimina signos como @
Trans_review$Translated_Review <- gsub("([^[:alpha:] ]+|&amp;|&am;|\\n+)", "", Trans_review$Translated_Review)
# Eliminacion de signos de puntuación
Trans_review$Translated_Review <- gsub("[[:punct:]]", "", Trans_review$Translated_Review)
# Pasar de mayuscula a minuscula
Trans_review$Translated_Review <- tolower(Trans_review$Translated_Review)


corpus.tmp <- Corpus(VectorSource(Trans_review$Translated_Review))
#dtm <- DocumentTermMatrix(corpus.tmp)
dtm <- DocumentTermMatrix(corpus.tmp, list(wordLengths= c(3,12)))
inspect(dtm)

dtm <- removeSparseTerms(dtm, sparse= 0.99)
inspect(dtm)
tf <- as.matrix(dtm)

tfidf_DT <- suppressWarnings(weightTfIdf(dtm))
terms_DT <- tfidf_DT$dimnames$Terms
id1 <- 1
id2 <- 2
doc1 <- as.vector(tfidf_DT[ , id1])
names(doc1) <- terms_DT
doc2 <- as.vector(tfidf_DT[ , id2])
names(doc2) <- terms_DT
