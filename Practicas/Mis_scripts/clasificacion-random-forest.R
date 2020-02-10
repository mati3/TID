library(RTextTools)

# si no está disponilbe
# https://cran.r-project.org/src/contrib/Archive/maxent/  descargar 1.3.3.1
# https://cran.r-project.org/src/contrib/Archive/RTextTools/ descargar 1.4.2


c=sample(2,nrow(user_reviews),replace=TRUE,prob=c(0.7,0.3))
data_train <- user_reviews[c==1,]
data_test <- user_reviews[c==2,]


# logistica
modelo.logit <- glm(data_train$label ~ ., 
                    data = data_train, family = "binomial")
exp(coefficients(modelo.logit))

log.odds <- predict(modelo.logit, data.frame(SentimentNeutral = 0.6,
                                             SentimentPositive = 0.75))
