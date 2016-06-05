library(shiny)
library(memoise)
library(qdap)
library(stringi)
library(ggplot2)

options(shiny.maxRequestSize = 30 * 1024 ^ 2) #30MB; shiny default is 5MB.
#options(shiny.maxRequestSize = -1) #Large files

msg <- 0
if (!exists("train_sentence2"))
    load(file = "train_sentence2.RData")

##=#============================================================================
## general 1000 uk words
## ref: http://www.bckelk.ukfsn.org/words/uk1000n.html
#system("cat uk1000words.txt | awk '{print $1, $2, $3, $4}' > common_words.txt")
#common_words <- read.delim2("common_words.txt",sep=" ", stringsAsFactors = FALSE);
## the similar set is Fry_1000 words

single_word_result <- function (last_wrd_in_srch_string) {
    ## grep words in common_words; if it exists, then, next predicted word is the next indx word.
    #wrd <- check_spelling(last_wrd_in_srch_string)$suggestion ## correcting the spelling
    
    #if(check_spelling(last_wrd_in_srch_string)$not.found) return("previous-word-meaningless")
    
    wrd <-
        check_spelling(last_wrd_in_srch_string)$suggestion ## correcting the spelling
    print(wrd)
    if (is.null(wrd))
        return("previous-word-meaningless")
    
    ptrn <- paste0("^", wrd)
    
    ## if it does not exist, check qdapDictionaries::GradyAugmented
    ## from qdapDictionaries
    indx <-
        head(grep(ptrn, Fry_1000), n = 1)
    if (length(indx) < 1)
        indx <- head(grep(ptrn, GradyAugmented), n = 1)
    #if (length(indx) < 1)
    #    indx <- head(grep(ptrn, DICTIONARY), n = 1)
    if (length(indx) < 1)
        indx <- head(grep(ptrn, function.words), n = 1)
    if (length(indx) < 1)
        indx <- head(grep(ptrn, strong.words), n = 1)
    if (length(indx) < 1)
        indx <- head(grep(ptrn, submit.words), n = 1)
    if (length(indx) < 1)
        indx <- head(grep(ptrn, weak.words), n = 1)
    if (length(indx) < 1)
        indx <- head(grep(ptrn, negation.words), n = 1)
    if (length(indx) < 1)
        indx <- head(grep(ptrn, negative.words), n = 1)
    if (length(indx) < 1)
        indx <- head(grep(ptrn, positive.words), n = 1)
    
    if (length(indx) >= 1) {
        indx <- ((indx + 1) %% 1000) ## getting the next word from Fry_1000
        word <- Fry_1000[indx]
    } else {
        word <- "junkyard"
    }
}
## or check if its action.verbs, if so, display a preposition
##=#============================================================================


source("twitter-n-google-search.R")
#prediction_from_twitter_google(srch_strg, twitter0_or_goole1)

## There are 3 things
## 1. text box empty
## 2. there are no results returned
## 3. results returned

shinyServer(function(input, output, session) {
    # observeEvent(input$go, {
    #     session$sendCustomMessage(type = 'testmessage',
    #                               message = 'go: Thank you for clicking')
    # }
    # input$Predict, {
    #     session$sendCustomMessage(type = 'testmessage',
    #                               message = 'Predict: Thank you for clicking')
    # })
    
    # prediction result set  ===================================================
    prediction_sent_result_set <- memoise(function(srch_str) {
        theword = ""
        
        if (srch_str == "" | is.null(srch_str))
            return(NULL)
        
        ## if that does not return results then after replace contraction
        srch_str1 <- replace_contraction(srch_str)
        #print(srch_str1)
        
        #we want only last 5 words, beyond that it is not making sense
        #whenever we want this, we can change it
        list_srch_str <-
            tail(unlist(stri_extract_all_words(srch_str1)), n = 5)
        total_num_of_words <- length(list_srch_str)
        #print(list_srch_str)
        
        ## the last word should be present in the sentence, else, there is no point.
        ## also, this being the scenario, just get only those records, which has last word.
        last_word_in_search_str <- list_srch_str[total_num_of_words]
        tmp_db <-
            train_sentence2[grep(paste0(last_word_in_search_str, " \\w+"),
                                 train_sentence2)]
        
        ## Iterative process of the entire string you want to search
        for (start_cnt in 1:total_num_of_words) {
            #print(paste0(start_cnt," : ", Sys.time()))
            
            tmp_str <-
                stri_flatten(list_srch_str[start_cnt:total_num_of_words], collapse = " ")
            pattern_str <- paste0(".*", tmp_str, " (\\w+).*")
            #print(paste(start_cnt, tmp_str, pattern_str))
            
            indx <- grep(pattern_str, tmp_db) ##same case
            if (length(indx) < 1)
                indx <-
                grep(pattern_str, tmp_db, ignore.case = T) ##ignore case
            
            if (length(indx) >= 1) {
                res <- gsub(pattern_str, "\\1", tmp_db[indx], ignore.case = T)
                words_results <-
                    as.data.frame.table(table(res), stringsAsFactors = FALSE)
                words_results <-
                    words_results[order(words_results$Freq, decreasing = T), ]
                return(head(words_results, n = 50))
            }
        }
        if (length(indx) < 1) {
            print("am here")
            #We can do a spell check here itself and check offline data and if it exists return
            ## but for spelling error, we will penalize the user; due to lack of time.
            ## We will implement this next phase.
            
            t <-
                data.frame(
                    res = single_word_result(last_word_in_search_str),
                    Freq = 1,
                    stringsAsFactors = F
                )
            print(t)
            return(t)
        }
    })
    
    # reactive expression ======================================================
    # Define a reactive expression for the predictive word
    # randomVals <- eventReactive(input$Predict, {
    #     runif(input$srch_text)
    # })
    
    # reactive - prediction result set  ========================================
    #prediction_result_set <- eventReactive(input$Predict, {
    prediction_result_set <- reactive({
        #prediction_result_set <- function() {
        #print("am here ...")
        srch_txt <- input$srch_text
        withProgress({
            if (is.null(srch_txt) || srch_txt == "") {
                if(msg > 0) {
                    createAlert(
                        session,
                        "alert_anchor",
                        "empty_text",
                        title = paste(strong("No phrase!")),
                        content = "Enter a phrase to predict.",
                        dismiss = TRUE,
                        append = FALSE
                    )
                } else {
                msg <- 1
                createAlert(
                        session,
                        "alert_anchor",
                        "empty_text",
                        title = "",
                        content = "",
                        dismiss = TRUE,
                        append = FALSE
                    )
                closeAlert(session, "empty_text")
                }
                
                # session$sendCustomMessage(type = 'testmessage',
                #           message = 'Enter a phrase to predict.')
            }
            else {
                closeAlert(session, "empty_text")
                
                setProgress(message = "prediction in-progress ...")
                
                ### this is here, so both next word and graph gets the results.
                ## also, based on source choices, data gets sourced
                if (input$inRadio == "default") {
                    result_set <- prediction_sent_result_set(srch_txt)
                } else {
                    result_set <- tg_prediction_result_set(srch_txt, input$inRadio)
                }
                #result_set <- prediction_sent_result_set(srch_txt)
                result_set
            }
        })
        
    })
    
    # predict word function  ===================================================
    # predict_sent_next_word <- function()
    # {
    #     result_set <- prediction_result_set()
    #     print(paste("rs--",result_set))
    #     print(result_set)
    #     #if (is.null(result_set)) return("")
    #     ifelse(dim(result_set)[1] >= 1, result_set[1,]$res, result_set)
    # }
    
    # output the word  =========================================================
    output$theword <- renderText({
        result_set <- prediction_result_set()
        
        word <- ""
        if (!is.null(result_set)) {
            word <- ifelse(dim(result_set)[1] >= 1, result_set[1,]$res, NULL)
            print(result_set[1,]$res)
        }
        
        #pred_sent <- paste(input$srch_text, word)
        pred_sent <-
            HTML(paste(input$srch_text, "<strong>", word, "</strong>"))
        
        # alert - no results  ==================================================
        if (word == "" & stri_length(input$srch_text) >= 1) {
            closeAlert(session, "null_result")
            #print(input$srch_text)
            #print(gsub(".* (\\w+)[[:space:][:punct:]]*$","\\1",input$srch_text))
            createAlert(
                session,
                "alert_anchor",
                "null_result",
                title = paste(strong("No results!")),
                content = paste("Change the word '", em(
                    gsub(
                        ".* (\\w+)[[:space:][:punct:]]*$",
                        "\\1",
                        input$srch_text
                    )
                ), "' and continue."),
                dismiss = TRUE,
                append = FALSE
            )
        } else {
            closeAlert(session, "null_result")
        }
        
        # alert - exceed input size  ===========================================
        if (stri_length(paste(input$srch_text, word)) > 150) {
            pred_sent <- stri_sub(paste(input$srch_text, word), length = 150)
            createAlert(
                session,
                "alert_anchor",
                "exceed_char",
                title = paste(strong("Oops")),
                content = "You exceeded 150 characters limit!",
                dismiss = TRUE,
                append = FALSE
            )
        } else {
            closeAlert(session, "exceed_char")
        }
        
        # update phrase with predicted word, as you type  ======================
        #updateTextInput(session, "srch_text", value = pred_sent)
        
        # return the prediction sentence  ======================================
        pred_sent
    })
    
    # Turncate long words  =====================================================
    long_words_trunc <- function(wrd) {
        if(stri_length(wrd) > 7) wrd <- paste0(gsub("^(.?.?.?.?.?.?.?).*","\\1", wrd),"…")
        wrd
    }
    
    # 1. output the first word  ==================================================
    output$firstword <- renderText({
        result_set <- prediction_result_set()
        ifelse(dim(result_set)[1] >= 1, long_words_trunc(result_set[1,]$res), "")
    })
    
    # 2. output the second word  =================================================
    output$secword <- renderText({
        result_set <- prediction_result_set()
        ifelse(dim(result_set)[1] >= 2, long_words_trunc(result_set[2,]$res), "")
        
    })
    
    # 3. output the third word  ==================================================
    output$thirdword <- renderText({
        result_set <- prediction_result_set()
        ifelse(dim(result_set)[1] >= 3, long_words_trunc(result_set[3,]$res), "")
    })
    
    # 4. output the pipes between words  =========================================
    output$pipe1 <- renderText({
        "|"
    })
    output$pipe2 <- renderText({
        "|"
    })
    
    # Plot =====================================================================
    output$distPlot <- renderPlot({
        result_set <- prediction_result_set()
        print(result_set)
        if (!is.null(result_set)) {
            g1 <-
                ggplot (head(result_set, n = input$words),
                        aes(x = reorder(res, -Freq), y = Freq)) +
                geom_bar(stat = "Identity", fill = "lightgray") +
                geom_text(
                    aes (label = Freq) ,
                    vjust = -0.20,
                    size = 3,
                    angle = 45 ,
                    hjust = 0.5
                ) +
                xlab("Predict words list") +
                ylab("Frequency") +
                theme (axis.text.x = element_text (angle = 45 , hjust = 1))
            
            if (dim(result_set)[1] >= 1)
                print(g1)
        }
        #hist(result_set$Freq, col = 'darkgray', border = 'white')
    })
    
    #  ========================================================================+
    # twitter/google ==========================================================+
    #  ========================================================================+
    
    # observe({
    #     session$sendCustomMessage(
    #         type = 'testmessage',
    #         message = "test message"
    #     )
    # })
    
    observeEvent(input$inRadio, {
        if (input$inRadio == 0)
            session$sendCustomMessage(type = 'testmessage',
            message = 'Twitter may have authentication problem; if so, no results will be returned.')
    })
})
