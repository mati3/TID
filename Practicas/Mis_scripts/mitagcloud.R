
library(tm)
library(SnowballC)
library(wordcloud)
library(data.table)

lang <- "en"
analyseText = function(fileDir, lang, wordCount){
  data <- fread(fileDir,header=FALSE,sep = ",",data.table=T)
  
  stopWords2 <- c("yay","break.","next.","havent","couldnt","amp","isnt","get","just","got","dont","will","didnt","that","thats","yeah","ive","wont","yet","suck","sucks","gonna","haha","does","doesnt","cant","can")
  stopWords3 <- c("srccontentimagessiteimagesemoticonssmileycoolgif","srccontentimagessiteimagesemoticonssmileywinkgif","srccontentimagessiteimagesemoticonssmileylaughinggif","srccontentimagessiteimagesemoticonssmileysmilegif","srccontentimagessiteimagesemoticonssmileytongueoutgif","srccontentimagessiteimagesemoticonssmileysealedgif","albus","jajajajajaja","jajajaj","jajaj","hola","weeeeno","ok","tan","img","ala","man","men","je","jeje","jejeje","jejejeje","jejejejeje","ja","jaja","jajaja","jajajaja","jajajajaja","")
  
  
  if(lang == "en")   {stopWordsList <-c(stopwords("english"),stopWords2)
  } else  {stopWordsList <-c(stopwords("spanish"),stopWords3)}
  # analyse text and generate matrix of words
  # Returns a dataframe containing 1 tweet per row, one word per column
  # and the number of times the word appears per tweet
  CorpusTranscript <- Corpus(VectorSource(data$V2))
  CorpusTranscript <- tm_map(CorpusTranscript, tolower)
  # CorpusTranscript <- tm_map(CorpusTranscript, PlainTextDocument)
  CorpusTranscript <- tm_map(CorpusTranscript, removePunctuation)
  #CorpusTranscript <- tm_map(CorpusTranscript, content_transformer(function(x, pattern) gsub(pattern, "", x)), "srccontentimagessiteimagesemoticons\\w* ")
  CorpusTranscript <- tm_map(CorpusTranscript, removeWords, stopWordsList)
  CorpusTranscript <- tm_map(CorpusTranscript, removeNumbers)
  #CorpusTranscript <- tm_map(CorpusTranscript, removeWords, stopwords("spanish"))
  #CorpusTranscript <- tm_map(CorpusTranscript, stemDocument)
  CorpusTranscript <- DocumentTermMatrix(CorpusTranscript)
  CorpusTranscript <- removeSparseTerms(CorpusTranscript, 0.995) # keeps a matrix 995% sparse
  CorpusTranscript = as.data.frame(as.matrix(CorpusTranscript))
  colnames(CorpusTranscript) <- make.names(colnames(CorpusTranscript))
  
   freqWords <- colSums(CorpusTranscript)
   freqWords <- freqWords[order(freqWords, decreasing = T)]
   wordcloud(freq = as.vector(freqWords), words = names(freqWords),random.order = FALSE, colors = brewer.pal(8, 'Dark2'),max.words =wordCount)
}

analyseText("/home/mati/datospro2store.csv","en",100)
