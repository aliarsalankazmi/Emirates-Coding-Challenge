#############################################
###### Formulating Datasets for models ######
#############################################

# Loading packages
source("C:\\Users\\paperspace\\Documents\\Emirates Coding Challenge\\script/loadLibs.R")
library(rtweet)

em_dp <- "C:\\Users\\paperspace\\Documents\\Emirates Coding Challenge\\data\\emirates"
nEm_dp <- "C:\\Users\\paperspace\\Documents\\Emirates Coding Challenge\\data\\non-emirates"

modelPath <- list.files(em_dp, full.names = T, pattern = "lstm.+hdf5$")
secondModelPath <- list.files(em_dp, full.names = T, pattern = "ConvLstm.+hdf5$")
thirdModelPath <- list.files(em_dp, full.names = T, pattern = "BiLstm.+hdf5$")
emTwPath <- list.files(em_dp, full.names = T, pattern = "emiratesTweets.rds")
tokenizerPath <- list.files(nEm_dp, full.names = T, pattern = "cTextTokenizer")

lstmModel <- load_model_hdf5(modelPath)
convLstmModel <- load_model_hdf5(secondModelPath)
biLstmModel <- load_model_hdf5(thirdModelPath)
cTextTokenizer <- load_text_tokenizer(tokenizerPath)
emDf <- readRDS(emTwPath)

setDT(emDf)
emDf[, cText := plain_tweets(text)]
emDf[, cText := gsub("!", " ! ", cText)]
emDf[, cText := gsub("\\.", " \\. ", cText)]
emDf[, cText := gsub("\\?", " \\? ", cText)]
emDf[, cText := trimws(cText, "both")]
emDf[, cText := gsub('\\s+', ' ', cText)]

allSeqTexts <- texts_to_sequences(cTextTokenizer, emDf$cText)
allSeqTexts_m <- pad_sequences(allSeqTexts, 23) #this 23 is the max word length that we specified in our training

preds <- predict(lstmModel, allSeqTexts_m)
preds2 <- predict(convLstmModel, allSeqTexts_m)
preds3 <- predict(biLstmModel, allSeqTexts_m)

allPreds <- data.frame(pred1 = round(preds), pred2 = round(preds2), pred3 = round(preds3))
setDT(allPreds)
allPreds[, finalPred := rowSums(.SD)]
allPreds[, finalPredLbl := ifelse(finalPred > 0, 1, 0)]

saveRDS(allPreds, paste(em_dp, "predictions.rds", sep = "/"))



