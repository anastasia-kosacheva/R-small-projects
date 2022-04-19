# Loading packages
install.packages("twitteR")
library(twitteR)
library(stringr)
install.packages("RSQLite")
library(RSQLite)

# Writing down keys
api_key = "***"
api_secret = "***"
access_token = "***"
access_token_secret = "***"

# Authentication
setup_twitter_oauth(api_key, api_secret, access_token, access_token_secret)

# Connection to the database
con = dbConnect(SQLite(), dbname="Petersburg.db")
register_sqlite_backend("Petersburg.sqlite")

# Searching for tweets by geocode
first_twiter <- searchTwitter(searchString = "",n=1000, lang = "ru", geocode = '59.9971001,30.3769949,5km')

# Storing the data to data base
store_tweets_db(first_twiter)
spb_db = load_tweets_db()

# Creating data frame
spb_db = lapply(first_twiter, function(x){x$toDataFrame()})
spb_db = do.call(rbind, spb_db) 

# Getting data
spb_db = sapply(first_twiter, function(x) list(name = x$getScreenName(),
                                                   text = x$getText(), 
                                                   retweet_count = x$getRetweetCount()),
                                                   lng = x$getLongitude(),
                                                   lat = x$getLatitude(),
                                                   time = x$getCreated())
rm()
spb_db = t(spb_db)

# Creating data frame
spb_db = as.data.frame(spb_db)

# Cleaning text data
spb_db$text = str_replace_all(spb_db$text, "RT", "")
spb_db$text_clean = str_replace_all(spb_db$text, "@.+? ", "")
spb_db$text_clean <- gsub("[[:punct:]]", " ", spb_db$text_clean)
spb_db$text_clean <- gsub("[0-9]", " ", spb_db$text_clean)
spb_db$text_clean <- gsub("[^[:alnum:]]", " ", spb_db$text_clean)
spb_db$text_clean <- gsub("https", " ", spb_db$text_clean)

# Disconnecting from the database
dbDisconnect()

# Writing down the data
write.csv(spb_db,"spb_tw.csv")

# Loading packages
require(twitteR)
require(plyr)

# Getting API URL
URL = paste('http://search.twitter.com/search.atom?q=','&geocode=59.93428,30.3351,40km','&rpp=100&page=', page, sep='') 

# Getting table data from API URL
XML = htmlTreeParse(URL, useInternal=TRUE)
entry = getNodeSet(XML, "//entry")
tweets = c()

# Storing the data
for (i in 1:99){ 
  t = unlist(xpathApply(entry[[i]], "//title", xmlValue))
  tweets = c(tweets,t)
}

N=2000  # tweets to request from each query
S=3  # radius in miles

# Geolocation of the tweets
lats=df$y
lons=df$x

# Searching for tweets
spbtw=do.call(rbind,lapply(1:length(lats), function(i) searchTwitter('',
                                                                      lang="ru",n=N,resultType="recent",
                                                                      geocode=paste(lats[i],lons[i],paste0(S,"mi"),sep=","))))

# Applying latitude of tweets
spbtwlat=sapply(spbtw, function(x) as.numeric(x$getLatitude()))
spbtwlat=sapply(spbtwlat, function(z) ifelse(length(z)==0,NA,z))  

# Applying longitude of tweets
spbtwlon=sapply(spbtw, function(x) as.numeric(x$getLongitude()))
spbtwlon=sapply(spbtwlon, function(z) ifelse(length(z)==0,NA,z))  

# Applying date of tweets
spbtwdate=lapply(spbtw, function(x) x$getCreated())
spbtwdate=sapply(spbtwdate,function(x) strftime(x, format="%Y-%m-%d %H:%M:%S",tz = "UTC"))

# Getting text of tweets
spbtwtext=sapply(spbtw, function(x) x$getText())
spbtwtext=unlist(spbtwtext)

# Getting retweets
isretweet=sapply(spbtw, function(x) x$getIsRetweet())
retweeted=sapply(spbtw, function(x) x$getRetweeted())
retweetcount=sapply(spbtw, function(x) x$getRetweetCount())

# Getting number of favoured tweets
favoritecount=sapply(spbtw, function(x) x$getFavoriteCount())
favorited=sapply(spbtw, function(x) x$getFavorited())

# Creating data frame from binding data
data=as.data.frame(cbind(tweet=spbtwtext,date=spbtwdate,lat=spbtwlat,lon=spbtwlon,
                         isretweet=isretweet,retweeted=retweeted, retweetcount=retweetcount,favoritecount=favoritecount,favorited=favorited))


# Create corpus
corpus=Corpus(VectorSource(data$tweet))

# Convert to lower-case
corpus=tm_map(corpus,tolower)

# Remove stopwords
corpus=tm_map(corpus,function(x) removeWords(x,stopwords()))

# Convert corpus to a Plain Text Document
corpus=tm_map(corpus,PlainTextDocument)

# Getting word cloud of tweets with geodata
col=brewer.pal(6,"Dark2")
wordcloud(corpus, min.freq=25, scale=c(5,2),rot.per = 0.25,
          random.color=T, max.word=45, random.order=F,colors=col)
