result <<- NULL

predict <- function(string) {
        
        # cleaning up the string input
        string <- replace_ordinal(string)
        string <- replace_number(string, remove = TRUE)
        string <- replace_abbreviation(string)
        string <- replace_contraction(string)
        string <- replace_symbol(string, 
                                 pound = F, 
                                 at = F) 
        string <- qdap::qprep(string)
        
        # formatting of the string
        words <- str_to_lower(string) %>%
                str_remove_all(pattern = "[:punct:]") %>%
                str_split(pattern = " ")
        
        # check length of input
        wordslength <- length(words[[1]])
        
        if(wordslength >= 5){ 
                start <- wordslength - 3 
                } else start <- 1
        
        end <- wordslength
        selection <- words[[1]][start:end]
        str(selection)
        lookupnumber <- length(selection)
        
        # Showing the words that is broken up from string input
        print(paste("Lookup: ", selection))
        
        # Sorting the ngram according to MLE values
        
        startlookup <- lookupnumber + 1
        print(paste("Start level:", startlookup))
        
        ngramlookup <- function(selection, level){
                
                boo <- FALSE
                lookup <- str_c(selection, collapse = " ")
                
                # Fivegram Lookup
                if(level == 5){ 
                        print("Looking up fivegram")
                        
                        # indexing fivegram with highest MLE
                        prediction <- fivegram_lookup[frontwords == lookup][1]$follow
                        
                        if(!is.na(prediction)){ # if found
                                print("Found selection in Fivegram!")
                                print("Alternative prediction:")
                                result <<- head(fivegram_lookup[frontwords == lookup], 10)
                                print(result)
                                return(list(prediction, result))
                                break
                        }
                        else {
                                print("Backing down to level 4!!")
                                level <- 4
                                boo <- TRUE
                        }
                }
                # fourgram lookup
                if(level == 4){ 
                        print("Looking up in Fourgram")
                        
                        if(boo){
                                # shorten search string
                                lookup <- str_c(str_split(lookup, pattern = " ")[[1]][2:4], collapse = " ") # remove last word from search string
                                last <- str_c(str_split(lookup, pattern = " ")[[1]][4], collapse = " ")
                                print(paste("New lookup:", lookup))
                                
                                # retrieve matching fourgrams
                                fourgrams <- fourgram_lookup[frontwords == lookup]
                                str(fourgrams)
                                print("Old values:")
                                print(fourgrams[follow == last, .(frontwords, follow, mle)])
                                # reducing failed fivegram
                                fourgrams[follow == last, mle := 0.4 * mle] 
                                print("New values:")
                                print(fourgrams[follow == last, .(frontwords, follow, mle)])
                        }
                        else {
                                fourgrams <- fourgram_lookup[frontwords == lookup]
                        }
                        
                        # indexing fourgram with highest MLE
                        prediction <- fourgrams[1]$follow
                        print(paste("Prediction:", prediction))
                        
                        if(!is.na(prediction)){
                                print("Found Selection in Fourgram!")
                                print("Alternative preditions:")
                                result <<- head(fourgrams[frontwords == lookup], 10)
                                print(result)
                                return(list(prediction, result))
                                break
                        }
                        else {
                                level <- 3
                                boo <- TRUE
                        }
                        
                }
                # trigram lookup
                if(level == 3){ 
                        print("Looking up in trigram.")
                        
                        if(boo){
                                # shorten search string
                                lookup <- str_c(str_split(lookup, pattern = " ")[[1]][2:3], collapse = " ") # remove last word from search string
                                last <- str_c(str_split(lookup, pattern = " ")[[1]][3], collapse = " ")
                                print(paste("New lookup:", lookup))
                                
                                # retrieve matching trigrams
                                trigrams <- trigram_lookup[frontwords == lookup]
                                str(trigrams)
                                print("Old values:")
                                print(trigrams[follow == last, .(frontwords, follow, mle)])
                                # reducing failed fourgram
                                trigrams[follow == last, mle := 0.4 * mle]
                                print("New values:")
                                print(trigrams[follow == last, .(frontwords, follow, mle)])
                        }
                        else {
                                trigrams <- trigram_lookup[frontwords == lookup]
                        }
                        
                        # indexing trigram with highest MLE
                        prediction <- trigrams[1]$follow
                        print(paste("Prediction:", prediction))
                        
                        if(!is.na(prediction)){
                                print("Found Selection in Trigram")
                                print("Alternative prediction:")
                                result <<- head(trigrams[frontwords == lookup], 10)
                                print(result)
                                return(list(prediction, result))
                                break
                        }
                        else {
                                level <- 2
                                boo <- TRUE
                        }
                }
                # bigram lookup
                if(level == 2){ 
                        print("Looking up selection in Bigram!")
                        
                        if(boo){
                                # shorten search string
                                lookup <- str_c(str_split(lookup, pattern = " ")[[1]][1:1], collapse = " ") # remove last word from search string
                                last <- str_c(str_split(lookup, pattern = " ")[[1]][2], collapse = " ")
                                print(paste("New lookup:", lookup))
                                
                                # retrieve matching bigrams
                                bigrams <- bigram_lookup[frontwords == lookup]
                                print("Old values:")
                                print(bigrams[follow == last, .(frontwords, follow, mle)])
                                
                                # reducing failed trigram
                                bigrams[follow == last, mle := 0.4 * mle]
                                print("New values:")
                                print(bigrams[follow == last, .(frontwords, follow, mle)])
                        }
                        else {
                                bigrams <- bigram_lookup[frontwords == lookup]
                        }
                        
                        # lookup bigram with highest MLE
                        prediction <- bigrams[1]$follow
                        print(paste("Prediction:", prediction))
                        
                        if(!is.na(prediction)){
                                print("FOUND selectionin bigram !")
                                print("Alternative prediction:")
                                result <<- head(bigrams[frontwords == lookup], 10)
                                print(result)
                                return(list(prediction, result))
                                break
                        }
                        else {
                                result<<- NULL
                                print(result)
                                print("Please try again with other words.")
                        }
                }
                
        }
        
        ngramlookup(selection, startlookup)
        
} 
