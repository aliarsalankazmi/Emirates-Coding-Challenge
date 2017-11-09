#############################################
###### Loading packages & file paths #######
#############################################

pkgs <- c("data.table", "ggplot2", "dplyr", "stringr", "purrr", "bit64", "hrbrthemes", "scales", 
		"openNLP", "NLP", "spacyr", "tidytext", "keras", "caret", "tidytext")
loadedPkgs <- sapply(pkgs, require, character.only = T)
# checking all pkgs loaded
all(loadedPkgs)

dp <- "C:\\Users\\paperspace\\Documents\\Emirates Coding Challenge\\data\\non-emirates"
f1 <- list.files(dp, full.names = T, pattern = "^Tweets.*csv$")

#spacy_initialize()


