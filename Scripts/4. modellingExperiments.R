#############################################
###### Loading packages, files, & file paths #######
#############################################

source("C:\\Users\\paperspace\\Documents\\Emirates Coding Challenge\\script/loadLibs.R")

em_dp <- "C:\\Users\\paperspace\\Documents\\Emirates Coding Challenge\\data\\emirates"
nEm_dp <- "C:\\Users\\paperspace\\Documents\\Emirates Coding Challenge\\data\\non-emirates"

textTrainData <- readRDS(paste(nEm_dp, "textTrainData.rds", sep = "/"))
posTrainData <- readRDS(paste(nEm_dp, "posTrainData.rds", sep = "/"))
textTestData <- readRDS(paste(nEm_dp, "textTestData.rds", sep = "/"))
posTestData <- readRDS(paste(nEm_dp, "posTestData.rds", sep = "/"))

trainLbl <- readRDS(paste(nEm_dp, "trainLbl.rds", sep = "/"))
testLbl <- readRDS(paste(nEm_dp, "testLbl.rds", sep = "/"))

cTextTokenizer <- load_text_tokenizer(paste(nEm_dp, "cTextTokenizer", sep = "/"))
posTokenizer <- load_text_tokenizer(paste(nEm_dp, "posTokenizer", sep = "/"))

textMaxLen <-  23
posMaxLen <-  25
textVocab <-  12335
posVocab <-  15


# Specifying parameters for our models
lossVar <- seq(from = .1, to = .6, by = .1)
batchVar <- seq(from = 5, to = 45, by = 10)
unitsVar <- seq(from = 5, to = 95, by = 10)
embeddOutput <- seq(from = 50, to = 450, by = 100)
paramsDf <- expand.grid(lossVar, batchVar, unitsVar, embeddOutput)
# The above take a long time to be trained on models (3-4 days!) so we further trim them:

lossVar <- c(.2, .4, .6)
batchVar <- c(5, 20, 40)
unitsVar <- c(5, 20, 40, 90)
embeddOutput <- c(50, 100, 200)
paramsDf <- expand.grid(lossVar, batchVar, unitsVar, embeddOutput)
# Even these are taking a long time! Trim further!

lossVar <- c(.2, .4, .6)
batchVar <- c(5, 20, 40)
unitsVar <- c(5, 40, 90)
paramsDf <- expand.grid(lossVar, batchVar, unitsVar)

# LSTM Text Model
lstmModel_TextTrainer <- function(trainData, trainDataLbl, testData, testDataLbl,
				  vocabSize, batchVar, units, loss){
model <- keras_model_sequential()
model %>%
  layer_embedding(input_dim = vocabSize, output_dim = 200) %>% 
  layer_lstm(units = units, dropout = loss, recurrent_dropout = loss) %>% 
  layer_dense(units = 1, activation = 'sigmoid')
model %>% compile(loss = 'binary_crossentropy', optimizer = 'adam', metrics = c('accuracy'))
model %>% fit(trainData, trainDataLbl, batch_size = batchVar, epochs = 5, validation_data = list(testData, testDataLbl))
scores <- model %>% evaluate(testData, testDataLbl, batch_size = batchVar)
return(scores)
}

res_lstmTxtModel <- vector(mode = "list", length = nrow(paramsDf))

for(i in 1:nrow(paramsDf)){
params <- paramsDf[i,]
res_lstmTxtModel[[i]] <- lstmModel_TextTrainer(textTrainData, trainLbl, textTestData, testLbl,
						textVocab, batchVar = params$Var2, units = params$Var3, 
						loss = params$Var1)
}

res_lstmTxtModelDf <- lapply(res_lstmTxtModel, as.data.frame) %>% rbindlist %>% setDT
res_lstmTxtModelDf[, ':='(lossVar = paramsDf$Var1, batchVar = paramsDf$Var2, unitsVar = paramsDf$Var3)]
res_lstmTxtModelDf[, ':='(modelType = "LSTM_Text", modelName = 1:.N)]
saveRDS(res_lstmTxtModelDf, paste(em_dp, "modelExperiment_lstmTxt.rds", sep = "/"))

# BiDirectional LSTM
lstmBiModel_TextTrainer <- function(trainData, trainDataLbl, testData, testDataLbl,
				  vocabSize, batchVar, units, loss){
model <- keras_model_sequential()
model %>%
  layer_embedding(input_dim = vocabSize, output_dim = 200) %>% 
  bidirectional(layer_lstm(units = units)) %>% 
  layer_dropout(rate = loss) %>%
  layer_dense(units = 1, activation = 'sigmoid')
model %>% compile(loss = 'binary_crossentropy', optimizer = 'adam', metrics = c('accuracy'))
model %>% fit(trainData, trainDataLbl, batch_size = batchVar, epochs = 5, validation_data = list(testData, testDataLbl))
scores <- model %>% evaluate(testData, testDataLbl, batch_size = batchVar)
return(scores)
}

res_lstmBiTxtModel <- vector(mode = "list", length = nrow(paramsDf))

for(i in 1:nrow(paramsDf)){
params <- paramsDf[i,]
res_lstmBiTxtModel[[i]] <- lstmBiModel_TextTrainer(textTrainData, trainLbl, textTestData, testLbl,
						textVocab, batchVar = params$Var2, units = params$Var3, 
						loss = params$Var1)
}

res_lstmBiTxtModelDf <- lapply(res_lstmBiTxtModel, as.data.frame) %>% rbindlist %>% setDT
res_lstmBiTxtModelDf[, ':='(lossVar = paramsDf$Var1, batchVar = paramsDf$Var2, unitsVar = paramsDf$Var3)]
res_lstmBiTxtModelDf[, ':='(modelType = "Bi_LSTM_Text", modelName = 1:.N)]
saveRDS(res_lstmBiTxtModelDf, paste(em_dp, "modelExperiment_BiLstmTxt.rds", sep = "/"))

# Convolutional LSTM
lstmConvModel_TextTrainer <- function(trainData, trainDataLbl, testData, testDataLbl,
				  vocabSize, batchVar, units, loss){
model <- keras_model_sequential()
model %>%
  layer_embedding(input_dim = vocabSize, output_dim = 200) %>% 
  layer_conv_1d(filters = 3, kernel_size = 3) %>%
  layer_max_pooling_1d() %>%
  layer_lstm(units = units) %>% 
  layer_dropout(rate = loss) %>%
  layer_dense(units = 1, activation = 'sigmoid')
model %>% compile(loss = 'binary_crossentropy', optimizer = 'adam', metrics = c('accuracy'))
model %>% fit(trainData, trainDataLbl, batch_size = batchVar, epochs = 5, validation_data = list(testData, testDataLbl))
scores <- model %>% evaluate(testData, testDataLbl, batch_size = batchVar)
return(scores)
}

res_lstmConvTxtModel <- vector(mode = "list", length = nrow(paramsDf))

for(i in 1:nrow(paramsDf)){
params <- paramsDf[i,]
res_lstmConvTxtModel[[i]] <- lstmConvModel_TextTrainer(textTrainData, trainLbl, textTestData, testLbl,
						textVocab, batchVar = params$Var2, units = params$Var3, 
						loss = params$Var1)
}

res_lstmConvTxtModelDf <- lapply(res_lstmConvTxtModel, as.data.frame) %>% rbindlist %>% setDT
res_lstmConvTxtModelDf[, ':='(lossVar = paramsDf$Var1, batchVar = paramsDf$Var2, unitsVar = paramsDf$Var3)]
res_lstmConvTxtModelDf[, ':='(modelType = "Conv_LSTM_Text", modelName = 1:.N)]
saveRDS(res_lstmConvTxtModelDf, paste(em_dp, "modelExperiment_ConvLstmTxt.rds", sep = "/"))


# Now plotting results to check the best one
rf <- list.files(em_dp, full.names = T, pattern = "modelExperiment")
dfRes <- lapply(rf, readRDS) %>% rbindlist

ggplot(data = dfRes, aes(x = as.factor(modelName), y = acc, colour = modelType)) +
	geom_point(size = 2) +
	theme_ipsum()


# Model 8 for LSTMs seems to be the better one, so let's train using that model 
lstmModel <- keras_model_sequential()
lstmModel %>%
  layer_embedding(input_dim = textVocab, output_dim = 200) %>% 
  layer_lstm(units = 5, dropout = .4, recurrent_dropout = .4) %>% 
  layer_dense(units = 1, activation = 'sigmoid')
lstmModel %>% compile(loss = 'binary_crossentropy', optimizer = 'adam', metrics = c('accuracy'))
lstmModel %>% fit(textTrainData, trainLbl, batch_size = 40, epochs = 7, 
			validation_data = list(textTestData, testLbl))
scores <- lstmModel %>% evaluate(textTestData, testLbl, batch_size = 40)
save_model_hdf5(lstmModel, paste(em_dp, "lstmTextModel.hdf5", sep = "/"))

# Since there is a high probability that this model may be learning too much from US tweets, I am also saving
# a lesser accurate model - #20 for Conv LSTM
convLstmModel <- keras_model_sequential()
convLstmModel %>%
  layer_embedding(input_dim = textVocab, output_dim = 200) %>% 
  layer_conv_1d(filters = 3, kernel_size = 3) %>%
  layer_max_pooling_1d() %>%
  layer_lstm(units = 90) %>% 
  layer_dropout(rate = .4) %>%
  layer_dense(units = 1, activation = 'sigmoid')
convLstmModel %>% compile(loss = 'binary_crossentropy', optimizer = 'adam', metrics = c('accuracy'))
convLstmModel %>% fit(textTrainData, trainLbl, batch_size = 5, epochs = 5, 
				validation_data = list(textTestData, testLbl))
scores <- convLstmModel %>% evaluate(textTestData, testLbl, batch_size = 5)
save_model_hdf5(convLstmModel, paste(em_dp, "ConvLstmTextModel.hdf5", sep = "/"))

# And another model for BiLSTM - model # 3
biLstmModel <- keras_model_sequential()
biLstmModel %>%
  layer_embedding(input_dim = textVocab, output_dim = 200) %>% 
  bidirectional(layer_lstm(units = 5)) %>% 
  layer_dropout(rate = .6) %>%
  layer_dense(units = 1, activation = 'sigmoid')
biLstmModel %>% compile(loss = 'binary_crossentropy', optimizer = 'adam', metrics = c('accuracy'))
biLstmModel %>% fit(textTrainData, trainLbl, batch_size = 5, epochs = 5, 
				validation_data = list(textTestData, testLbl))
scores <- biLstmModel %>% evaluate(textTestData, testLbl, batch_size = 5)
save_model_hdf5(biLstmModel, paste(em_dp, "BiLstmTextModel.hdf5", sep = "/"))

