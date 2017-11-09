#Load packages
pkgs <- c("rtweet", "data.table", "dplyr")
loaded <- sapply(pkgs, require, character.only = T)
all(loaded)

#File paths and URLs
op <- "C:\\Users\\paperspace\\Documents\\Emirates Coding Challenge\\data\\emirates"

#Keys
consumer_key = ""
consumer_secret = ""
tw_token <- create_token(consumer_key = consumer_key, consumer_secret = consumer_secret)


dfTE <- search_tweets(q = "@emirates", type = "recent", include_rts = FALSE, 
		n = 20000, retryonratelimit = TRUE, token = tw_token, lang = "en")
setDT(dfTE)
saveRDS(dfTE, paste(op, "emiratesTweets.rds", sep = "/"))


