############################################################################################################
## Sequence		: 03a
## File			: twitter-n-google-search.R
## Author		: Ambika J
## Date			: May-2016
## Notes		: This file is to search "Live" - using twitter and google.
##					1. Live twitter search
##					2. Live google search
############################################################################################################

####################################
## Live Twitter and google search
####################################

library(memoise)
library(twitteR)
library(RCurl)
library(XML)
#require(ROAuth)

twit_auth1 <- function() {
    ## a small workaround to get the hidden files to shiny app
    if(file.exists("twit_file.RData"))
        system("mv twit_file.RData .twit_file.RData; mv twito.RData .twito.RData")
    
    #print("moved")
    #system("ls -la | grep twit")
    
    options(httr_oauth_cache=TRUE)
    if(!exists("api_key")) {
        file1 <-".twit_file.RData"
        load(file1); load(file)
    }
    #print(file)

    setup_twitter_oauth(api_key, api_secret, access_token, access_token_secret)
    #rm(list = c("api_key","api_secret","access_token","access_token_secret"));
    #system("cp .twit_file.RData twit_file.RData; cp .twito.RData twito.RData")
}

twit_auth <- function() {
    source("twit_auth.R")
}


### Logic:
# 1. search twitter for the search string
# 2. convert search results to a data frame
# 3. clean the last word which 3 dots (example: what is this my...; here "my..." will be deleted)
# 4. pattern to replace; cleaning the tweets
# 5. return the cleaned twit

live_extract_from_twitter <- memoise(function(srch_string) {
	#srch_string <- "Be grateful for the good times and keep the faith during the"
	searchTwit     <- suppressWarnings(try(searchTwitter(srch_string), silent = TRUE))
	#print("1")
	if (class(searchTwit) == "try-error" & length(grep("OAuth", searchTwit))) {
	    print("2")
	    twit_auth()
	    searchTwit     <- suppressWarnings(searchTwitter(srch_string))
	}
	print(searchTwit[1])
	print("twit3")
	if(is.null(searchTwit)) return(NULL)
	
	#if (length(searchTwit) < 1) searchTwit <- userTimeline("sassyy_dallas",sinceID="503037502974726143",maxID="503037502974726145")
	extractTwit_df <- twListToDF(searchTwit)
	extractTwit    <- gsub(" ([[:punct:][:alnum:]])*\\.\\.\\.","",extractTwit_df$text)
	pattern_rplc   <- paste0("[[:print:]]*(",srch_string," *)",sep="")
	cleanTwit      <- gsub(pattern_rplc,"\\1",extractTwit)
	return(cleanTwit)
})

### Logic:
# 1. create search url with the search string specified
# 2. pass the search url and get the html page's content
# 3. use html tree parse to parse it as nodes
# 4. search the nodes for span and class 'st'
# 5. this would return more than one result and there is no way to return all of that at once.
#		so, through a FOR loop add each node to a list
# 6. return this list

google_search <- memoise(function(srch_string) {
  #s <- "I'll be there for you, I'd live and I'd"
  # take the variable "s" and paste it into a google search url
  search.url   <- paste("https://www.google.co.in/search?q=%22",gsub(" ","+",srch_string),"%22",sep="")
  # grab the html contents of the search results page
  search.html  <- getURL(search.url)
  # format the html contents
  parse.search <- htmlTreeParse(search.html,useInternalNodes = TRUE)
  # find a span with the class "st"
  search.nodes <- getNodeSet(parse.search,"//span[@class='st']")
  
  search.value <- ""
  for (i in 1:length(search.nodes)) search.value[i] <- xmlValue(search.nodes[[i]])

  return(search.value)
})

####################################
## Twitter and google search to predict
####################################

### Logic:
# Parameters: (1) search string and (2) whether to use twitter [0] or google [1] to search
# 1. clean search string, by removing punctuation and extra spaces.
# 2. extract the results from twitter or google (as per the choice)
# 3. if there is an error; PREDICTION would be 
#		(1) internet not working: predict "<internet-not-working>"
#		(2) twitter not working:  predict "<do-google-search>"
#			(it is either twitter or google; but google being down is next to impossible for now)
# 4. if there are no errors, it has some results from twitter or google.
#		a. clean these results, by removing punctuation and extra spaces.
#		b. grep the lines which has the search string + one word
#			then, extract out that one word only.
#		c. if there is any space while extracting, remove it off.
#		d. if it does not return results, then, it might be end of sentence.
#			predict: "<end-of-sentence>"
#		e. else
#			if it return a blank record, then, predict: "<end-of-sentence>"
#			else
#				calculate the frequencies of the words extracted
#				extract the highest frequency from that set.

prediction_from_twitter_google <- function(srch_strg, twitter0_or_goole1) {
    print("4")
	srch_strg <- gsub(" $", "", gsub("[[:space:]]+"," ",gsub("[[:punct:]]"," ",srch_strg)))
	
	if (twitter0_or_goole1 == 0 )  { tt <- try(live_extract_from_twitter(srch_strg), silent=TRUE)
	} else {  tt <- try(google_search(srch_strg), silent=TRUE) }
	
	if (class(tt) == "try-error" | is.null(tt)) {
	    print("error/null 4")
	    print(tt)
		### check if the internet connection is ON/OFF; a dirty way tho
		## If you do not use "== TRUE", then, it throws the internet connection error, which is not intended.
		#results <- ifelse(try(is.character(getURL("www.google.com")), silent = TRUE) == TRUE, "<do-google-search>", "<internet-not-working>")
	    return(NULL)
	} else {
		tt <- gsub("[[:space:]]+"," ",gsub("[[:punct:]]"," ",tt))
		print("valid")
        print(tt[1])
        
		##srch_strg <- tolower(srch_strg)
		ptrn <- paste0(".*", srch_strg, "[[:space:]]*(\\w+)+.*")

		results <- gsub(ptrn,"\\1", grep(paste0(srch_strg," \\w+"), tt, value=T, 
		                                 ignore.case=T), ignore.case=T)
		results <- gsub("^[[:space:]]","",results)
		#print(results)
		return(results)
	}
}
	
	
tg_prediction_result_set <- function(srch_strg, twitter0_or_goole1) {
    res <- prediction_from_twitter_google(srch_strg, twitter0_or_goole1)
    
	if (length(res) == 0) return(NULL) 
	if (length(res) == 1 && sort(unique(res)) == "") return(NULL)

	words_results <- as.data.frame.table(table(res), stringsAsFactors = FALSE)
	words_results <- words_results[order(words_results$Freq, decreasing = T), ]
	head(words_results, n=50)
}

tg_predict_word <- function(srch_strg, twitter0_or_goole1) {
    result_set <- prediction_result_set(srch_strg, twitter0_or_goole1)
    
    if(is.null(result_set)) return("")
    
    ifelse(dim(result_set)[1] >= 1, result_set[1,]$res, NULL)
}

