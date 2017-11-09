#############################################
###### Loading packages & file paths #######
#############################################

source("C:\\Users\\paperspace\\Documents\\Emirates Coding Challenge\\script/loadLibs.R")
library(topicmodels)
library(tidytext)
library(rtweet)

em_dp <- "C:\\Users\\paperspace\\Documents\\Emirates Coding Challenge\\data\\emirates"
emTwDf <- readRDS(paste(em_dp, "emiratesTweets.rds", sep = "/"))
emTwDf[, cTweets := plain_tweets(text)]

emDtm <- 
	emTwDf[, .(status_id, cTweets)] %>%
		unnest_tokens(word, cTweets) %>%
		anti_join(stop_words) %>%
  		count(status_id, word, sort = TRUE) %>%
  		ungroup() %>%
		cast_dtm(status_id, word, n)

# Let's run topic modelling for number of topics = 2:20. We will then identify which has > log lik. and pick that
topicModels <- lapply(2:20, function(d) LDA(emDtm, d))
topicModels_log <- as.data.frame(as.matrix(lapply(topicModels, logLik)))
topicModels_Df <- data.frame(topics=c(2:20), LL = as.numeric(as.matrix(topicModels_log)))

# Plot log likelihoods to see which number of topics to use - seems like 5 is our number:
ggplot(topicModels_Df, aes(x = topics, y = LL)) + 
    xlab("Number of topics") + 
    ylab("Log likelihood of the model") + 
    geom_line() + 
    theme_bw() 

bestModel <- LDA(emDtm, 5)
termsDf <- as.data.frame(get_terms(bestModel, 15))
saveRDS(termsDf, paste(em_dp, "topicModel.rds", sep = "/"))

