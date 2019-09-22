
library(rtweet)
#requires a valid google API and a twitter account

## stream tweets from london for 60 seconds
gkey="AIzaSyCAIKyBV88cNnTr42-49nsk5XLVBEI6fKs"

rt <- stream_tweets(lookup_coords("london, uk"), timeout = 60)

lookup_coords("St. Paul, MN",apikey=gkey)

components = "postal_code"


re<-search_twitter_and_store(lookup_coords("london, uk"), timeout = 60)


rt <- search_tweets("lang:en", geocode = "44.9537,93.090,50mi", include_rts = FALSE)

#geocode = "44.9537,93.090,50mi"

rt <- search_tweets(q=c("st. paul AND crime"), n = 18000,include_rts = FALSE)

