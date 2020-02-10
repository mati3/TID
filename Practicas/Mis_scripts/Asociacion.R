# Apriori

# Data Preprocessing
# install.packages('arules')
library(arules)
dataset = read.csv('/home/mati/Trans_review.csv', header = FALSE)
dataset = read.transactions('/home/mati/Trans_review.csv', sep = ',', rm.duplicates = TRUE)
summary(dataset)
itemFrequencyPlot(dataset, topN = 10)

# Training Apriori on the dataset
rules = apriori(data = dataset, parameter = list(support = 0.00001, confidence = 0.2))
detach(package:tm, unload=TRUE)
library(arules)
# Visualising the results
inspect(sort(rules, by = 'lift')[1:10])

plot(rules, method="graph")
