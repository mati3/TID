# Factorizamos las columnas:
  
# cuando aparezca valor nan
#Sentiment = as.factor(datospro$Sentiment, levels = c("Negative","Neutral","Positive"))
#Type = as.factor(datospro$Type, levels = c("Free","Paid"))

Sentiment = as.factor(datospro$Sentiment)
Sentiment = factor(Sentiment, levels = c("Negative","Neutral","Positive"))
Category = as.factor(datospro$Category)
Review = as.factor(datospro$Translated_Review)
Type = as.factor(datospro$Type)
Type = factor(Type, levels = c("Free","Paid"))
Price = as.factor(datospro$Price)
Genres = as.factor(datospro$Genres)
Installs = as.factor(datospro$Installs)
Rating = as.factor(datospro$Content.Rating)
App = as.factor(datospro$App)
  
# visualmente

plot(Type,Sentiment, xlab="Type", ylab="Sentimientos")
plot(Review,Sentiment, xlab="Review", ylab="Sentimientos")
plot(Rating,Sentiment, xlab="Rating", ylab="Sentimientos")
plot(Price,Sentiment, xlab="Price", ylab="Sentimientos")
plot(Installs,Sentiment, xlab="Installs", ylab="Sentimientos")
plot(Genres,Sentiment, xlab="Genres", ylab="Sentimientos")
plot(Category,Sentiment, xlab="Category", ylab="Sentimientos")
plot(App,Sentiment, xlab="App", ylab="Sentimientos")
# No corresponde
#plot(Sentiment,datos2$Sentiment_Polarity)
#plot(datos$Sentiment_Subjectivity,datos$Sentiment_Polarity)