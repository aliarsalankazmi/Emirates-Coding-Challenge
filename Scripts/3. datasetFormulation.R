#############################################
###### Formulating Datasets for models ######
#############################################

# Loading packages
source("C:\\Users\\paperspace\\Documents\\Emirates Coding Challenge\\script/loadLibs.R")

dp <- "C:\\Users\\paperspace\\Documents\\Emirates Coding Challenge\\data\\non-emirates"
f1 <- list.files(dp, full.names = T, pattern = "^.+2\\.rds$")
dfTw1 <- readRDS(f1)


# Just minor cleaning/fix in preparation for modelling
dfTw2 <- dfTw1[airline_sentiment != "neutral", 
	.(cText, posText, posText2, airline_sentiment, airline_sentiment_confidence)]
dfTw2[, cText := gsub("!", " ! ", cText)]
dfTw2[, cText := gsub("\\.", " \\. ", cText)]
dfTw2[, cText := gsub("\\?", " \\? ", cText)]
dfTw2[, cText := trimws(cText, "both")]
dfTw2[, cText := gsub('\\s+', ' ', cText)]

# Adding a new version of tweet text, in case we decide to use this instead
# The only difference in cText2 from cText is that the former does not contain punctuation
dfTw2[, cText2 := gsub("[[:punct:]]", "", cText)]
dfTw2[, cText2 := trimws(cText2, 'both')]
dfTw2[, cText2 := gsub('\\s+', ' ', cText2)]

# Make the sentiment label numeric
dfTw2[, sentiment := ifelse(airline_sentiment == "negative", 0, 1)]

# Creating training and test sets now, but with stratified sampling so that we mirror actual dist. of each class
trainIndex <- createDataPartition(as.factor(dfTw2$airline_sentiment), p = .7, list = FALSE)

# Tokenising, vectorising, and changing text to sequences with padding
# For this, we need to know the length of tweets and POS tags
dfTw2[, .(tweet_id = 1:.N, cText)] %>%
	unnest_tokens(words, cText) %>%
	group_by(tweet_id) %>%
	summarise(totalWords = n()) %>%
	ggplot(data = ., aes(x = totalWords)) + 
	geom_histogram() +
  	ggtitle("Length of Tweets in Words") +
  	labs(x = "Length of Tweets", y = "Counts") +
	theme_ipsum()

dfTw2[, .(tweet_id = 1:.N, posText)] %>%
	unnest_tokens(words, posText) %>%
	group_by(tweet_id) %>%
	summarise(totalPosTags = n()) %>%
	ggplot(data = ., aes(x = totalPosTags)) + 
	geom_histogram() +
  	ggtitle("Length of Tweets in POS Tags") +
  	labs(x = "Length of Tweets", y = "Counts") +
	theme_ipsum()

# Let us set word length to 23 and POS tag length to 25
# And what about the vocabulary size of tweets and POS tags?

vocabCText <- dfTw2[, .(tweet_id = 1:.N, cText)] %>%
    		unnest_tokens(words, cText) %>% 
		summarise(n_distinct(words)) %>%
		unlist

vocabPos <- dfTw2[, .(tweet_id = 1:.N, posText)] %>%
    		unnest_tokens(posTags, posText) %>% 
		summarise(n_distinct(posTags)) %>% 
		unlist

maxWordLen <- 23
maxPosLen <- 25

cTextTokenizer <- text_tokenizer(num_words = vocabCText, filters = "#$%&()*+,-./:;<=>@[\\]^_`{|}~\t\n", lower = TRUE,
				split = " ", char_level = FALSE)	
posTokenizer <- text_tokenizer(num_words = vocabPos, split = " ", char_level = FALSE)

fit_text_tokenizer(cTextTokenizer, dfTw2$cText)
fit_text_tokenizer(posTokenizer, dfTw2$posText)

save_text_tokenizer(cTextTokenizer, paste(dp, "cTextTokenizer", sep = "/"))
save_text_tokenizer(posTokenizer, paste(dp, "posTokenizer", sep = "/"))

allSeqTexts <- texts_to_sequences(cTextTokenizer, dfTw2$cText)
allSeqTexts_m <- pad_sequences(allSeqTexts, maxWordLen)
allSeqPos <- texts_to_sequences(posTokenizer, dfTw2$posText)
allSeqPos_m <- pad_sequences(allSeqPos, maxPosLen)

textTrainData <- allSeqTexts_m[trainIndex,]
posTrainData <- allSeqPos_m[trainIndex,]
textTestData <- allSeqTexts_m[-trainIndex,]
posTestData <- allSeqPos_m[-trainIndex,]

#This is not needed now. Initially, it was being used as we tried modelling 3 classes...
#trainLbl <- to_categorical(dfTw2$sentiment[trainIndex])
#testLbl <- to_categorical(dfTw2$sentiment[-trainIndex])

trainLbl <- dfTw2$sentiment[trainIndex]
testLbl <- dfTw2$sentiment[-trainIndex]

saveRDS(textTrainData, paste(dp, "textTrainData.rds", sep = "/"))
saveRDS(posTrainData, paste(dp, "posTrainData.rds", sep = "/"))
saveRDS(textTestData, paste(dp, "textTestData.rds", sep = "/"))
saveRDS(posTestData, paste(dp, "posTestData.rds", sep = "/"))
saveRDS(trainLbl, paste(dp, "trainLbl.rds", sep = "/"))
saveRDS(testLbl, paste(dp, "testLbl.rds", sep = "/"))



