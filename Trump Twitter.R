#This is the code we use in this session. 

# package twitteR
install.packages("twitteR")
library("twitteR")

install.packages("ROAuth")
library("ROAuth")

install.packages("RCurl")
library("RCurl")

# from the Twitter developer account
key = "LESTdVPLihtetk5b83bRXWoJI"

secret = "Km4y8GMJgQNcxeD3ZgWO1mLhWbtf6GsulP2S7ITtKrrBxw0ppo"

# set a working directory 

setwd("C:\\Users\\Noel\\Documents\\College\\B8IT108 Data and Web Mining\\Twitter")

# Cacert.pem is a collection of certificates

download.file(url="http://curl.haxx.se/ca/cacert.pem", 
              destfile="C:\\Users\\Noel\\Documents\\College\\B8IT108 Data and Web Mining\\Twitter\\cacert.pem",
              method="auto")

# we are entering the whole Twitter API info and call the whole object authenticate
authenticate <-  OAuthFactory$new(consumerKey=key,
                                  consumerSecret=secret,
                                  requestURL='https://api.twitter.com/oauth/request_token',
                                  accessURL='https://api.twitter.com/oauth/access_token',
                                  authURL='https://api.twitter.com/oauth/authorize')


authenticate$handshake(cainfo="C:\\Users\\Noel\\Documents\\College\\B8IT108 Data and Web Mining\\Twitter\\cacert.pem")

# insert the PIN from Twitter
6049317


save(authenticate, file="twitter authentication.Rdata")

#registerTwitterOAuth(authenticate)


api_key <- "LESTdVPLihtetk5b83bRXWoJI"
api_secret <- "Km4y8GMJgQNcxeD3ZgWO1mLhWbtf6GsulP2S7ITtKrrBxw0ppo"
access_token <- "15034179-TSGs05Rr8puYICXhRGT5jF2w87eHdasOP8AGFSOQk"
access_token_secret <- "bHyEqfbGjjRZJXSyDmENEILeF83PY4UFce8JvGbRdnNLr"

setup_twitter_oauth(api_key, api_secret, access_token, access_token_secret)

# now start to analyse a particular Twitter account.
userTimeline("realDonaldTrump")

#userTimeline("realDonaldTrump", cainfo="cacert.pem")

# searchTwitter is the main function of the package

# we are now scraping 200 tweets for Trump, and we als specify our certificate
TrumpTwitts = searchTwitter("#realDonaldTrump", n=100)
TrumpTwitts


class(TrumpTwitts)
length(TrumpTwitts)
head(TrumpTwitts)

install.packages("tm")
library("tm")


Trumplist <- sapply(TrumpTwitts, function(x) x$getText()) 

Trumplist

# use the corpus function. A corpus is the text body consisting of all the text including the meta info

Trumpcorpus <- Corpus(VectorSource(Trumplist))

# putting text to lower case
Trumpcorpus <- tm_map(Trumpcorpus, tolower) 

# remove punct
Trumpcorpus <- tm_map(Trumpcorpus, removePunctuation) 

# remove stopwords (meaningless words)
Trumpcorpus <- tm_map(Trumpcorpus,function(x)removeWords(x,stopwords())) 

install.packages("wordcloud")
library("wordcloud")

wordcloud(Trumpcorpus, min.freq=5, scale=c(5,1),random.color=F, max.word=15, random.order=F)

# changing to a tdm
Trumptdm <- TermDocumentMatrix(Trumpcorpus)

# a DocumentTermMatrix is a very useful tool when it comes to text mining
# it structures the text in a matrix where each term is organized in a column.each row is a document and the number represents the counts of that term

# frequent terms
findFreqTerms(Trumptdm, lowfreq=11)

# Lets get a dendrogram to see related terms
# Remove sparse (infrequently used) terms from the term-document matrix
Trump2tdm <-removeSparseTerms(Trumptdm, sparse=0.9)

findFreqTerms(Trump2tdm, lowfreq=11)

# Lets scale the data
Trump2tdmscale <- scale(Trump2tdm)

# distance matrix
Trumpdist <- dist(Trump2tdmscale, method = "euclidean")

# hierarchical clustering
Trumpfit <- hclust(Trumpdist)

# Visualize the result
plot(Trumpfit)

# to calculate a certain number of groups
cutree(Trumpfit, k=6)

# we can even color the 6 groups and plot them
rect.hclust(Trumpfit, k=6, border="red")























================================================================================
  Trumplist <- sapply(TrumpTwitts, function(x) x$getText()) 
Trumpcorpus <- Corpus(VectorSource(Trumplist),  readerControl=list(language="english"))

# use the corpus function. A corpus is the text body consisting of all the text including the meta info

Trumpcorpus <- tm_map(Trumpcorpus, function(x) iconv(x, to='UTF-8', sub='byte'))

# putting text to lower case
Trumpcorpus <- tm_map(Trumpcorpus, tolower) 

# remove punct
Trumpcorpus <- tm_map(Trumpcorpus, removePunctuation) 

