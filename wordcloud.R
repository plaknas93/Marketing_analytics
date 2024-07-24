
#Word cloud analytics

#Load libraries
library("tm")
library("SnowballC")
library("RColorBrewer")
library("wordcloud")
library("wordcloud2")

#Text mining

##Loading doc
filepath = "D:/RWorks/Marketing_analytics/Data/Loan_proposal_desperate.txt"

#filepath = "D:/RWorks/Marketing_analytics/Data/budget_speech-1.txt"

#filepath <- "http://www.sthda.com/sthda/RDoc/example-files/martin-luther-king-i-have-a-dream-speech.txt"
text = readLines(filepath)
docs <- Corpus(VectorSource(text))

##Inspect the content of the document
inspect(docs)

##Text transformation
toSpace = content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs = tm_map(docs, toSpace, "/")
docs = tm_map(docs, toSpace, "@")
docs = tm_map(docs, toSpace, "\\|")

##Cleaning the text

# Convert the text to lower case
docs = tm_map(docs, content_transformer(tolower))
# Remove numbers
docs = tm_map(docs, removeNumbers)
# Remove english common stopwords
docs = tm_map(docs, removeWords, stopwords("english"))

# Remove your own stop word
# specify your stopwords as a character vector
docs = tm_map(docs, removeWords, c("blabla1", "blabla2")) 

# Remove punctuations
docs = tm_map(docs, removePunctuation)
# Eliminate extra white spaces
docs = tm_map(docs, stripWhitespace)
# Text stemming
# docs <- tm_map(docs, stemDocument)

## Term document matrix
dtm = TermDocumentMatrix(docs)
m = as.matrix(dtm)
v = sort(rowSums(m),decreasing=TRUE)
d = data.frame(word = names(v),freq=v)
head(d, 10)

## Generate wordcloud
set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 2,max.words=100, random.order=FALSE, rot.per=0.20, colors=brewer.pal(7, "Dark2"))
