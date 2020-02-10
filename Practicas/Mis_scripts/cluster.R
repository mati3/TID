library(cluster)
library(factoextra)
library(dplyr)
library(fpc)
library(ggplot2)
library("corpus")
library(tm)


corpus.tmp <- Corpus(VectorSource(Trans_review))
#dtm <- DocumentTermMatrix(corpus.tmp)
dtm <- DocumentTermMatrix(corpus.tmp, list(wordLengths= c(3,12)))
inspect(dtm)

dtm <- removeSparseTerms(dtm, sparse= 0.99)
inspect(dtm)
tf <- as.matrix(dtm)

# Para estudiar la importancia de los términos de un documento en particular, en lugar de utilizar
#la frecuencia de cada uno de los términos directamente, se pueden utilizar diferentes ponderaciones
#(TF-IDF),  las cuales se calculan como el producto de dos medidas, la frecuencia de aparición del
#término (tf) y la frecuencia inversa del documento (idf).

# vector de pesos en idf
idf <- log(nrow(tf)/(colSums(tf!=0))+1)

# Matriz tf-idf
dtidf <- t(t(tf)*idf)
#####
#dist1=dist(dtidf)
#dist2=dist(dtidf,method="binary")
#Calculo distancia ponderada y Cluster
#distancia(dist1,dist2)
###

# Jerarquico

hc=hclust(dist(dtidf),method="ward.D2")
hc
#dibujo el dendrograma y corto por tres
plot(hc, labels = user_reviews$Sentiment)
rect.hclust(hc,k=3)
#genero la variable de agrupamiento
group=cutree(hc,k=3)
#Medidas de bondad de agrupamiento: coeficiente de silueta
plotcluster(dtidf,group)
#idx=sample(1:dim(dtidf)[1],300)
idx=sample(1:dim(dtidf)[1],150)
shi= silhouette(group[idx],dist(dtidf[idx]))
plot(shi,col=1:3)
#Calculo de algunas otras medidas de bondad del agrupamiento
#(Ver descripcion de la funcion)
cluster.stats(dist(dtidf),group,alt.clustering=as.integer(user_reviews$Sentiment))
  
# k-medias

kmeans.result=kmeans(dtidf,2)
kmeans.result
table(user_reviews$Sentiment,kmeans.result$cluster)
group=kmeans.result$cluster
plotcluster(dtidf,group)
idx=sample(1:dim(dtidf)[1],150)
shi= silhouette(group[idx],dist(dtidf[idx]))
plot(shi,col=1:3)
  fviz_nbclust (dtidf, kmeans, method = "silhouette")
#Calculo de algunas otras medidas de bondad del agrupamiento
cluster.stats(dist(dtidf),group,alt.clustering=as.integer(user_reviews$Sentiment))


# k-medoides

pam.result=pam(dist(dtidf),5)
idx=sample(1:dim(dtidf)[1],150)
grupo=pam.result$clustering
plotcluster(dtidf,grupo)
shi= silhouette(group[idx],dist(dtidf[idx]))
plot(shi,col=1:3)
cluster.stats(dist(dtidf[idx]),grupo[idx])

####################### otras pruebas ###########################################

corpus.tmp <- Corpus(VectorSource(Trans_review))
#dtm <- DocumentTermMatrix(corpus.tmp)
dtm <- DocumentTermMatrix(corpus.tmp, list(wordLengths= c(3,12)))
inspect(dtm)

dtm <- removeSparseTerms(dtm, sparse= 0.99)
inspect(dtm)
tf <- as.matrix(dtm)

  # Distancia coseno
tfidf_DT <- suppressWarnings(weightTfIdf(dtm))
terms_DT <- tfidf_DT$dimnames$Terms
id1 <- 1
id2 <- 2
doc1 <- as.vector(tfidf_DT[ , id1])
names(doc1) <- terms_DT
doc2 <- as.vector(tfidf_DT[ , id2])
names(doc2) <- terms_DT

distancia <- function(x, y){
  resultado <- x%*%y / (sqrt(x %*% x) * sqrt(y %*%y ))
  return(as.numeric(resultado))
}
distancia(doc1,doc2)


datos <- data.frame(sentimientos = rep(c("positivo", "negativo", "neutro"), each = 10),
                    valoracion = c(doc1, doc2),
                    item = 1:50)
# correlación
ggplot(data = datos, aes(x = as.factor(item), y = valoracion, colour = sentimientos)) +
  geom_path(aes(group = sentimientos)) +
  geom_point() +
  labs(x = "ítem") +
  theme_bw() +
  theme(legend.position = "bottom")

# correlación de una variable con el resto
findAssocs(dtm, "app", corlimit = 0.6)

# Distancia euclídea
  ggplot(data = datos, aes(x = as.factor(item), y = valoracion,
    colour = sentimientos)) +
    geom_path(aes(group = sentimientos)) +
    geom_point() +
    geom_line(aes(group = item), colour = "firebrick", linetype = "dashed") +
    labs(x = "item") +
    theme_bw() + theme(legend.position = "bottom")

