library("dplyr")

# Eliminar todas las filas que contengan algún valor nulo
delete.na <- function(df, n=0) {
     df[rowSums(is.na(df)) <= n,]
 }
user_reviews <- na.omit(googleplaystore_user_reviews)

# Dividimos nuestro dataset en 3, filtrando por sentimientos:
positive <- user_reviews %>% filter(Sentiment=="Positive") 
negative <- user_reviews %>% filter(Sentiment=="Negative") 
neutral <- user_reviews %>% filter(Sentiment=="Neutral") 

# Para positive:
# Generamos 1500 números al azar para positive que tiene 23998 muestras.
filas.random <- sample(1:23998, 1500, replace= F)
positive <- as.data.frame(positive[filas.random,])
# Para negative:
filas.random <- sample(1:8271, 1500, replace= F)
negative <- as.data.frame(negative[filas.random,])
# Para neutral:
filas.random <- sample(1:5158, 1500, replace= F)
neutral <- as.data.frame(neutral[filas.random,])
# Ahora unimos los review a googleplaystore y quitamos los duplicados
user_reviews = rbind(positive,negative,neutral)
user_reviews <- user_reviews[!duplicated(user_reviews), ]
datospro = merge(user_reviews, googleplaystore)
# Quitamos las filas que tienen valores nulos
datospro = na.omit(datospro)
# Quitamos repetidos
datospro <- datospro[!duplicated(datospro), ]

#Otra forma para que queden menos datos sería quitar duplicados donde se repita Translated_Review
#datospro <- datospro[!duplicated(datospro$Translated_Review), ]

#Descargamos los ficheros:
write.csv(datospro, file="datospro.csv")
