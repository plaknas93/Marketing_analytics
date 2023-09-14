
#Sentiment Analysis using R: Amazon.in reviews

##Steps: (1)Use "Amazon review exporter" to extract data (2)Text Mining and (3)Visualization

## Step 0: Import packages into R
library(tm) #text analysis aka text mining
library(wordcloud) 
library(syuzhet)

#library(lubridate)
library(ggplot2)
library(tibble)
library(tidyverse)
library(tidytext)
library(tidydata)
#library(scales)
#library(reshape2)
library(dplyr)

## Step 1: Import data into R
#reviews=read.csv(file="D:/RWorks/Marketing_analytics/Data/kotler_reviews.csv")
#reviews=read.csv(file="D:/RWorks/Marketing_analytics/Data/fireboult_reviews.csv")
reviews=read.csv(file="D:/RWorks/Marketing_analytics/Data/apple_2021_a13_reviews.csv")

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
w=subset(w,w>=15)
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
#write.csv(x=s,file="D:/RWorks/Marketing_analytics/Data/kotler_sentiment.csv")
write.csv(x=s,file="D:/RWorks/Marketing_analytics/Data/fireboult_sentiment.csv")



#Visulization
head(s)
emo_bar= colSums(s)
head(emo_bar)
emo_sum=data.frame(emotion=names(emo_bar),tot_score=emo_bar)
head(emo_sum)

ggplot(data = emo_sum,aes(x=emotion,y=tot_score))+geom_bar(stat='identity',fill= 'salmon',col= 'black')+ggtitle("Sentiment analysis for Noise Watch based on Amazon reviews")+xlab("emotion")+ylab("Sentiment score")

ggplot(data = emo_sum,aes(x=reorder(emotion,-tot_score),y=tot_score))+geom_bar(stat='identity',fill='salmon',col='black')+ggtitle("Sentiment analysis for Noise Watch based on Amazon reviews")+xlab("emotion")+ylab("Sentiment score")

barplot(colSums(prop.table(s[, 1:8])),space = 0.2,horiz = FALSE,las = 1,cex.names = 0.7,col = brewer.pal(n = 8, name = "Set3"), main = "Noise watch",sub = "Analysis by SPN",xlab="emotions", ylab = NULL)

#Analysis using tidytext package using bing lexicon
text.df=tibble(text=str_to_lower(reviews$text))
bing_word_counts=text.df %>% unnest_tokens(output=word,input=text) %>% inner_join(get_sentiments("bing")) %>% count(word, sentiment, sort=TRUE)

head(bing_word_counts)

##Select to 10 words by emotions
bing_top_10_words_by_sentiments=bing_word_counts %>% group_by(sentiment) %>% slice_max(order_by=n,n=10) %>% ungroup %>% mutate(word=reorder(word,n))
bing_top_10_words_by_sentiments

##Create a barplot of words contributing to sentiments
bing_top_10_words_by_sentiments %>% ggplot(aes(word,n,fill=sentiment))+geom_col(show.legend=FALSE)+facet_wrap(~sentiment,scales="free_y")+labs(y="Contribution to sentiment",x=NULL)+coord_flip()


#Analysis using tidytext package using loughran lexicon
loughran_word_counts=text.df %>% unnest_tokens(output=word,input=text) %>% inner_join(get_sentiments("loughran")) %>% count(word, sentiment, sort=TRUE)

head(loughran_word_counts)

##Select to 10 words by emotions
loughran_top_10_words_by_sentiments=loughran_word_counts %>% group_by(sentiment) %>% slice_max(order_by=n,n=10) %>% ungroup %>% mutate(word=reorder(word,n))
loughran_top_10_words_by_sentiments

##Create a barplot of words contributing to sentiments
loughran_top_10_words_by_sentiments %>% ggplot(aes(word,n,fill=sentiment))+geom_col(show.legend=FALSE)+facet_wrap(~sentiment,scales="free_y")+labs(y="Contribution to sentiment",x=NULL)+coord_flip()
