
#Twitter analytics

#Libraries
library(twitteR)
library(ROAuth)
library(hms)
library(lubridate) 
library(tidytext)
library(tm)
library(wordcloud)
library(igraph)
library(glue)
library(networkD3)
library(rtweet)
library(plyr)
library(stringr)
library(ggplot2)
library(ggeasy)
library(plotly)
library(dplyr) 
library(hms)
library(lubridate) 
library(magrittr)
library(tidyverse)
library(janeaustenr)
library(widyr)

#Note: Replace below with your credentials following above reference
api_key = "DH8MrgIsyIWsaZZdMNxjIPXNn"
api_secret = "iWIfCrXEu9q6yLdmawXgnuuRT5s4gc0rwVXtsWZSD7wM91edJj"
access_token = "1281505465646473216-5zKtSCHyh227Ez4gQVEvseyj6dCDMb"
access_token_secret = "ajUTkau7IFh1rHw3EKWnwnDSfFODSxMuOWCOW0o4Pva1g"

#Note: This will ask us permission for direct authentication, type '1' for yes:
setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)

# extracting 200 tweets related to "G20" topic
tweets = searchTwitter("#globalwarming", n=200, lang="en")
n.tweet = length(tweets)

# convert tweets to a data frame
tweets.df <- twListToDF(tweets)

tweets.txt <- sapply(tweets, function(t)t$getText())
# Ignore graphical Parameters to avoid input errors
tweets.txt <- str_replace_all(tweets.txt,"[^[:graph:]]", " ")

## pre-processing text:
clean.text = function(x)
{
  # convert to lower case
  x = tolower(x)
  # remove rt
  x = gsub("rt", "", x)
  # remove at
  x = gsub("@\\w+", "", x)
  # remove punctuation
  x = gsub("[[:punct:]]", "", x)
  # remove numbers
  x = gsub("[[:digit:]]", "", x)
  # remove links http
  x = gsub("http\\w+", "", x)
  # remove tabs
  x = gsub("[ |\t]{2,}", "", x)
  # remove blank spaces at the beginning
  x = gsub("^ ", "", x)
  # remove blank spaces at the end
  x = gsub(" $", "", x)
  # some other cleaning text
  x = gsub('https://','',x)
  x = gsub('http://','',x)
  x = gsub('[^[:graph:]]', ' ',x)
  x = gsub('[[:punct:]]', '', x)
  x = gsub('[[:cntrl:]]', '', x)
  x = gsub('\\d+', '', x)
  x = str_replace_all(x,"[^[:graph:]]", " ")
  return(x)
}

cleanText <- clean.text(tweets.txt)
# remove empty results (if any)
idx <- which(cleanText == " ")
cleanText <- cleanText[cleanText != " "]