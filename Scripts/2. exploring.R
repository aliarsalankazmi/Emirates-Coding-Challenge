##########################################################
###### Reading data, exploring it, and cleaning it #######
##########################################################

dfTw <- fread(f1)


str(dfTw)

# Let's check the proportion of data for each of the sentiment classes
dfTw[, .N, by = airline_sentiment
	][, propC := N/sum(N)] %>%
	ggplot(data = ., aes(x = airline_sentiment, y = propC)) + 
	geom_col() +
	scale_y_continuous(labels = percent) +
	theme_ipsum()

# Let's see how much confidence there is for each sentiment
ggplot(data = dfTw, aes(x = airline_sentiment_confidence)) +
geom_histogram() +
theme_ipsum() +
facet_wrap(~airline_sentiment)


# Are there any duplicate tweets? Well, there are...
dfTw[, .N, by =  text][N > 1]

# We don't care for the @.+ , so we can safely remove them
dfTw[, cText := gsub("@.+?\\b", "", text)]

# Remove ampersand, replace quotes such as " with '. Because quotes may often be used in sarcasm. We need to account this
# as a feature in our model
dfTw[, cText := gsub("&amp;", "", cText)]
dfTw[, cText := gsub("&quot;", " ' ", cText)]
dfTw[, cText := gsub('"'," ' ", cText)]

# We have some strange characters too, due to encoding... Try adjusting this to ASCII
dfTw[, cText := iconv(cText, to = "ASCII//TRANSLIT")]

# We're really looking to extrapolate outside of this dataset to emirates. Likely hurdles like 
## airline names
## geo settings
# will need to be removed, as they may introduce overfitting. We have a small dataset anyway!

# Removing airline names
dfTw[, cText := gsub("\\bvirgin\\b", "", cText, ignore.case = T)]
dfTw[, cText := gsub("\\bunited\\b", "", cText, ignore.case = T)]
dfTw[, cText := gsub("\\bsouthwest\\b", "", cText, ignore.case = T)]
dfTw[, cText := gsub("\\bdelta\\b", "", cText, ignore.case = T)]
dfTw[, cText := gsub("\\bus airways\\b", "", cText, ignore.case = T)]
dfTw[, cText := gsub("\\bamerican\\b", "", cText, ignore.case = T)]


# hashtags and number of hashtags can be bearers of sentiment... Let's check which hashtags are used so we can
# remove text about airlines but keep the hash and those hashtags related to other aspects of services
dfCheck <- strsplit(x = dfTw$cText, split = " ") %>% 
		unlist %>%
		data.table(tokens = .) %>%
		filter(grepl("#", tokens)) %>%
		mutate(token = tolower(tokens)) %>%
		group_by(token) %>%
		summarise(counts = n()) %>%
		arrange(-counts) %>%
		setDT
#dfCheck[1:50]

dfTw[, cText := gsub("#destinationdragons", "", cText, ignore.case = T)] #related to a concert!
dfTw[, cText := gsub("#avgeek", "", cText, ignore.case = T)] #related to aviation geek posts
dfTw[, cText := gsub("#jetblue", "#", cText, ignore.case = T)]
dfTw[, cText := gsub("#UnitedAirlines", "#", cText, ignore.case = T)]
dfTw[, cText := gsub("#usairwaysfail", "#fail", cText, ignore.case = T)]
dfTw[, cText := gsub("#AmericanAirlines", "#", cText, ignore.case = T)]
dfTw[, cText := gsub("#united", "#", cText, ignore.case = T)]
dfTw[, cText := gsub("#USAirways", "#", cText, ignore.case = T)]
dfTw[, cText := gsub("#usairwaysfail", "#fail", cText, ignore.case = T)]
dfTw[, cText := gsub("#dfw", "#", cText, ignore.case = T)]
dfTw[, cText := gsub("#jfk", "#", cText, ignore.case = T)]
dfTw[, cText := gsub("#unitedsucks", "#sucks", cText, ignore.case = T)]
dfTw[, cText := gsub("#flyfi", "#", cText, ignore.case = T)]
dfTw[, cText := gsub("#southwest", "#", cText, ignore.case = T)]
dfTw[, cText := gsub("#unitedfail", "#fail", cText, ignore.case = T)]
dfTw[, cText := gsub("#lufthansa", "#", cText, ignore.case = T)]
dfTw[, cText := gsub("#southwestairlines", "#", cText, ignore.case = T)]
dfTw[, cText := gsub("#dca", "#", cText, ignore.case = T)]
dfTw[, cText := gsub("#lax", "#", cText, ignore.case = T)]
dfTw[, cText := gsub("#lax", "#", cText, ignore.case = T)]
dfTw[, cText := gsub("#airlines", "#", cText, ignore.case = T)]
# Now that we have removed most hashtags that are related to airlines, whilst keeping the hashtags for other 
# aspects of service, and also keeping words like fail, we can now proceed to other parts of data cleaning 

# Let's check what nouns are being used in normal text - if these are related to airlines, they can bias our model
# So we need to remove them. 
# Here, we are using a POS tagger and Entity detection to help us...
infoWords <- spacy_parse(dfTw$cText, tag = T) %>% setDT
infoWords[, .N, by = entity]
infoWords[grepl("WORK_OF_ART", entity)]
infoWords[grepl("TIME", entity)]
infoWords[grepl("PERSON", entity)]
infoWords[grepl("MONEY", entity)]
infoWords[grepl("LOC", entity)]
infoWords[grepl("PRODUCT", entity)]

# So two things that I would really want to inspect:
infoWords[grepl("PERSON", entity)][, .N, by = .(lemma, entity)][order(-N)][1:40]
infoWords[grepl("ORG", entity)][, .N, by = .(lemma, entity)][order(-N)][1:40]

# Based on inspection from above code, I would remove the following from our data
dfTw[, cText := gsub("vegas|jetblue|david|karen|logan|terry|lauren", "", cText)]
dfTw[, cText := gsub("\\bunited\\b", "", cText)]
dfTw[, cText := gsub("\\bairways\\b", "", cText)]
dfTw[, cText := gsub("\\bairlines\\b", "", cText)]
dfTw[, cText := gsub("\\dc\\b", "", cText)]
dfTw[, cText := gsub("\\bus\\b", "", cText)]
dfTw[, cText := gsub("\\bdc\\b", "", cText)]
dfTw[, cText := gsub("\\busair\\b", "", cText)]
dfTw[, cText := gsub("\\bdfw\\b", "", cText)]
dfTw[, cText := gsub("\\bphl\\b", "", cText)]
dfTw[, cText := gsub("\\bord\\b", "", cText)]
dfTw[, cText := gsub("\\bclt\\b", "", cText)]
dfTw[, cText := gsub("\\bewr\\b", "", cText)]
dfTw[, cText := gsub("\\bsfo\\b", "", cText)]
dfTw[, cText := gsub("\\bua\\b", "", cText)]
dfTw[, cText := gsub("\\bnyc\\b", "", cText)]
dfTw[, cText := gsub("\\blax\\b", "", cText)]
dfTw[, cText := gsub("\\bbos\\b", "", cText)]
dfTw[, cText := gsub("\\blga\\b", "", cText)]
dfTw[, cText := gsub("\\bbwi\\b", "", cText)]
dfTw[, cText := gsub("\\bmco\\b", "", cText)]
dfTw[, cText := gsub("\\bphx\\b", "", cText)]
dfTw[, cText := gsub("\\biah\\b", "", cText)]
dfTw[, cText := gsub("\\bswa\\b", "", cText)]
dfTw[, cText := gsub("\\bbna\\b", "", cText)]
dfTw[, cText := gsub("\\batl\\b", "", cText)]
dfTw[, cText := gsub("\\bfll\\b", "", cText)]
dfTw[, cText := gsub("\\bjfk\\b", "", cText)]
dfTw[, cText := gsub("\\bden\\b", "", cText)]
dfTw[, cText := gsub("\\blas\\b", "", cText)]
dfTw[, cText := gsub("\\bpdx\\b", "", cText)]
dfTw[, cText := gsub("\\bfaa\\b", "", cText)]
dfTw[, cText := gsub("\\bdal\\b", "", cText)]
dfTw[, cText := gsub("\\bmia\\b", "", cText)]
dfTw[, cText := gsub("\\bdia\\b", "", cText)]
dfTw[, cText := gsub("\\brdu\\b", "", cText)]
dfTw[, cText := gsub("\\btsa\\b", "", cText)]
dfTw[, cText := gsub("\\bfl\\b", "", cText)]
dfTw[, cText := gsub("\\bamerican\\b", "", cText)]
dfTw[, cText := gsub("\\bdelta\\b", "", cText)]


# Let's save the data that also contains a cleaned text column...
saveRDS(dfTw, paste(dp, "data_cleaned_1.rds", sep = "/"))





###########################################################
###### Gather some more insights into variables  #######
##########################################################



# Re-load our data set and continue cleaning/exploring...
dfTw1 <- readRDS(paste(dp, "data_cleaned_1.rds", sep = "/"))


# Intuition is that negative and/or sarcastic tweets will have 
# 1. more hashtags, 
# 2. more capital letters,
# 3. specific length of tweets, 
# 4. and possibly specific patterns of POS, 
# so let us check some of these...

# 1. Number of hashtags
dfTw1[, numberOfHashtags := str_count(cText, "#")] %>%
	ggplot(data = ., aes(x = noH)) + 
	geom_histogram() +
	facet_wrap(~airline_sentiment) + 
	theme_ipsum()

# Let's check proportions
dfTw1[, Hashtags := ifelse(numberOfHashtags > 0, "More Than 0", "Less Than 0")
	][, .(counts = .N), by = .(airline_sentiment, Hashtags)
	][, totals := sum(counts), by = airline_sentiment
	][, proportions := counts/totals, by = airline_sentiment
	][order(airline_sentiment)] %>%
	ggplot(data = ., aes(x = Hashtags, y = proportions)) + 
	geom_col() +
	theme_ipsum() +
	ggtitle("What Proportion of tweets for each sentiment have more than 0 Hashtags?") +
	facet_wrap(~airline_sentiment)

# Boxplot
dfTw1 %>%
	ggplot(data = ., aes(x = airline_sentiment, y = numberOfHashtags)) + 
	geom_boxplot() +
	scale_y_log10() +
	theme_ipsum() +
  	ggtitle("Boxplot for understanding the distribution of the number of hashtags") +
	labs(y = "Number of Hashtags on Log-scale")


# Let's compare this with the original, unclean text to verify we have not caused any shifts by cleaning!
dfTw[, noH := str_count(text, "#")] %>%
	ggplot(data = ., aes(x = noH)) + 
	geom_histogram() +
	facet_wrap(~airline_sentiment) + 
	theme_ipsum()

dfTw %>%
	ggplot(data = ., aes(x = airline_sentiment, y = noH)) + 
	geom_boxplot() +
	scale_y_log10() +
	theme_ipsum()



# 2. Number of caps
dfTw1[, noC := sapply(regmatches(cText, gregexpr("[A-Z]", cText, perl=TRUE)), length), by = tweet_id] %>%
	ggplot(data = ., aes(x = noC)) + 
	geom_histogram() +
	facet_wrap(~airline_sentiment) +
	ggtitle("Do Negative Tweets have more Caps?") + 
  	labs(x = "Number of Capital Letters") +
	theme_ipsum()

dfTw1 %>% 
	ggplot(data = ., aes(x = airline_sentiment, y = noC)) + 
	geom_boxplot() +
	scale_y_log10() +
	ggtitle("Boxplot for distribution of the number of caps per sentiment") +
  	labs(x = "Number of Capital Letters on Log-scale") +
	theme_ipsum()



# 3. Length of tweets
dfTw1[, lenT := nchar(cText)] %>%
	ggplot(data = ., aes(x = lenT)) + 
	geom_histogram(binwidth = 10) +
	facet_wrap(~airline_sentiment) + 
	theme_ipsum()

dfTw1 %>%
	ggplot(data = ., aes(x = airline_sentiment, y = lenT)) + 
	geom_boxplot() +
	scale_y_log10() +
	theme_ipsum()


# Since we need to feed this to an RNN, we need to have similar dimensions of each sequence
# Just exploring the length of tweets

	ggplot(data = dfTw1, aes(x = lenT)) + 
	geom_histogram(binwidth = 10) + 
	theme_ipsum()

summary(dfTw1$lenT)
