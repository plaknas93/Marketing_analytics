
#Sentiment Analysis using R: Amazon.in reviews

##Steps: (1)Use "Amazon review exporter" to extract data (2)Text Mining and (3)Visualization

## Step 0: Import packages into R
library(tm) #text analysis aka text mining
library(wordcloud) 
library(syuzhet)

#library(lubridate)
library(ggplot2)
#library(scales)
#library(reshape2)
#library(dplyr)

## Step 1: Import data into R
reviews=read.csv(file="D:/RWorks/Marketing_analytics/Data/kotler_reviews.csv")
str(reviews) #structure of the data

#Creating Corpus - list of documents
corpus=reviews$text
corpus
corpus=Corpus(VectorSource(corpus))
corpus

#to see corpus
inspect(corpus[1:5])

#Cleaning corpus
corpus=tm_map(corpus, tolower) #all lower case
inspect(corpus[4])

corpus=tm_map(corpus, removePunctuation) #remove punctuation
inspect(corpus[4])

corpus=tm_map(corpus, removeNumbers) #remove numbers
inspect(corpus[4])

corpus=tm_map(corpus, removeWords,stopwords(kind = "en")) #remove stopwords
inspect(corpus[4])

corpus=tm_map(corpus, removeWords,c("book")) # remove common words not suitable for text analysis
inspect(corpus[4])

corpus=tm_map(corpus,stripWhitespace)
inspect(corpus[1:5])

reviews_final=corpus


## Step 2: Text Mining
#Create a term document
tdm=TermDocumentMatrix(reviews_final)
tdm=as.matrix(tdm)
tdm[1:10,1:5]

#Bar plot of words
w=rowSums(tdm)
w=subset(w,w>=10)
barplot(w, las=2,col='blue')

#wordcloud
w=sort(rowSums(tdm),decreasing = T)
set.seed(2000)
wordcloud(words=names(w),freq=w,max.words = 50,random.order = T,min.freq =5,colors = brewer.pal(25,"Dark2"),scale=c(3,0.3) )

#Sentiment scores
sentiment_data=iconv(reviews$text)
s=get_nrc_sentiment(sentiment_data)
s[1:10,]

#Calculate reviewwise score
s$score =s$positive-s$negative
s[1:10,]

#write to a csv file
write.csv(x=s,file="D:/RWorks/Marketing_analytics/Data/kotler_sentiment.csv")


#Visulization
ggplot(data = s,)