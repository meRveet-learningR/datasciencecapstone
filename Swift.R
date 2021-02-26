# Loading of Required Librarys
library(tidyr)
library(dplyr)
library(quanteda)
library(qdap)
library(ggplot2)
library(stringr)
library(tidytext)
library(data.table)
library(readr)

# Exploring Data, Loading Datasets and profanity list
blog<- readLines("final/en_US/en_US.blogs.txt", encoding="UTF-8", skipNul = T)
news<- readLines("final/en_US/en_US.news.txt", encoding="UTF-8", skipNul = T)
twitter<- readLines("final/en_US/en_US.twitter.txt", encoding="UTF-8", skipNul = T)
profanitylist<- readLines("https://raw.githubusercontent.com/LDNOOBW/List-of-Dirty-Naughty-Obscene-and-Otherwise-Bad-Words/master/en")

# Removing profanity from dataset
blogclean<- char_remove(blog, pattern= profanitylist, case_insensitive= T)
newsclean<- char_remove(news, pattern= profanitylist, case_insensitive= T)
twitterclean<- char_remove(twitter, pattern= profanitylist, case_insensitive= T)

# Functions is made to produce sample data set
# data<- source of raw data, #percent<- percentage of data to be used.
# Picking samples out of the dataset using Binomial sampling methods
# For assignment purpose, only 1% of the dataset is used to reduce calculation time required
# 1% of each data set would be approximately 1-2mb in size. 
samselection<- function(data, percent){
        z<- paste0(deparse(substitute(data)),"sam")
        set.seed(1)
        assign(z,data[as.logical(rbinom(length(data), 1, percent))], envir=globalenv())
}

# Storing Samples and Writing samples in text to prevent lost of data
samselection(blog, 0.01)
samselection(news, 0.01)        
samselection(twitter, 0.01)
dir.create("sample", showWarnings = F)
write(blogsam, "sample/blogsam.txt")
write(newssam, "sample/newssam.txt")
write(twittersam, "sample/twittersam.txt")

# Creating a function to replacement specific elements of the text using QDAP
cleaning<- function(clean){
        z<- paste0(deparse(substitute(clean)),"_final")
        # Replaces ordered numbers into text (e.g. 1st to first)
        clean<- replace_ordinal(clean)
        # Replace Numbers with Text
        clean<- replace_number(clean)
        # Replace Contractions to Long form (isn't to is not)
        clean<- replace_contraction(clean)
        # Replace Symbols with word equivalent (e.g. # to hex)
        clean<- replace_symbol(clean, pound= F, at = F)
        # Replace Abbreviations to full form (e.g. Mr. to Mister)
        clean<- replace_abbreviation(clean)
        # Adding of spaces after commas to prevent word concatenation
        clean<- comma_spacer(clean)
        # Removing non-symbol or unnecessary characters (e.g. _, ', 's)
        clean<- str_remove_all(clean, pattern= "\\_")
        clean<- str_remove_all(clean, pattern= "\\.")
        clean<- str_remove_all(clean, pattern= "\\'s")
        clean<- str_remove_all(clean, pattern= "\\,")
        clean<- str_remove_all(clean, pattern= "\\'")
        # Removing brackets and remaining symbols
        clean<- qprep(clean)
        # Storing the cleaned dataset in a variable
        assign(z, clean, envir=globalenv())
}

blogsam_final<- cleaning(blogsam)
newssam_final<- cleaning(newssam)
twittersam_final<- cleaning(twittersam)

 #Combing the dataset and creating corpus and spliting into training, hold and test sets.
corp<- corpus(c(blogsam_final,newssam_final,twittersam_final))
summary(corp)
setseed(1)
allindex<- 1:length(corp)
tindex<- sample(allindex,size=0.8*length(corp)) #80% as training set
hindex<- sample(allindex[-tindex], size=0.5*length(allindex[-tindex]))
testindex<- sample(allindex[-c(tindex,hindex)])
train<- corp[tindex]
hold<- corp[hindex]
test<- corp[testindex]

#Frequency of Text Used
dfmtrain<- dfm(train,
         remove = stopwords("english"), 
         remove_punct=T, 
         remove_symbols=T, 
         remove_numbers=T, 
         remove_separators=T, 
         remove_url=T,
         tolower = T)
textfreq<- textstat_frequency(dt)
textfreq
textfreqordered<- textfreq
textfreqordered$feature<- with(textfreq, reorder(feature, -frequency))
head(textfreq)
head(textfreqordered)

#Further Processing Functions 
#For sentence conversion
tokensconvert<- function(data){
        z<- paste0(deparse(substitute(data)),"_tokens")
        data<- tokens(char_tolower(data), 
                      remove_numbers = T, 
                      remove_punct = T, 
                      remove_symbols = T, 
                      remove_separators = T, 
                      split_hyphens = T, 
                      remove_url = T,
                      verbose = T) 
               assign(z, data, envir=globalenv())
}
tokensconvert(train)
train_tokens
##############################################
#Function to generate ngrams
ngramsconvert<- function(tokensconvert, ngram_count){
                y<- tokens_ngrams(tokensconvert, ngram_count, concatenator = " ")
        
        dt<- data.table(ngram= unlist(y))
}

train_1gram<- ngramsconvert(train_tokens, 1)
train_2gram<- ngramsconvert(train_tokens, 2)
train_3gram<- ngramsconvert(train_tokens, 3)
train_4gram<- ngramsconvert(train_tokens, 4)
train_5gram<- ngramsconvert(train_tokens, 5)

#Function to Split the last word from 2-5 ngrams

splitgrams<- function(ngram_dt, ngram_count){
        frontparts<- c("five", "four", "three", "two")
        start<- 6-ngram_count
        end<- ngram_count-1
        ngram_dt<- ngram_dt %>%
                separate(ngram, into= c(frontparts[start:4], "follow"), sep = " ", fill = "left") %>%
                unite(1:end, col= "frontwords", sep = " ")
        ngram_dt
}

y<- splitgrams(train_2gram, 2)

#MLE calculator function
mle<- function(ngram_dt, k, v){
        #counting the frequency of base for normalising
        # Adding counts of ngrams observed and by grouping them according to the frontwords and following words.
        # Counts were then filtered to >3 to fine tune the dataset observation
        dt<- ngram_dt[,.(follow, frontpartstotal= .N), by= .(frontwords)
                      ][,.(frontpartstotal, ngramct=.N), by=.(frontwords, follow)
                        ][ngramct>3
                          ][,.(frontwords, follow, frontpartstotal, ngramct, mle= (ngramct+k)/(frontpartstotal+(k*v))) ]
        unique(dt[!is.na(follow)])
}

mle(splitgrams(train_5gram, 5), k=1, v=length(unlist(splitgrams(train_5gram, 5))))
        
#################################
#Function to create ngram mle dt

ngrammledt<- function(tokens, name = "bigram", ngram_count, smoothvalue=1, v){
        tokens<- tokensconvert(tokens)
        ngram_dt<- ngramsconvert(tokens, ngram_count)
        str(ngram_dt)
        
        splitdt<- splitgrams(ngram_dt, ngram_count)
        str(splitdt)
        rm(ngram_dt)
       
        k<- smoothvalue
        v<- length(unique(unlist(tokens))) # vocab size from tokens
        
        mledt<-  mle(splitdt, smoothvalue, v)
        saveRDS(mledt, file = paste0(name, "_mle.rds"))
        rm(splitdt)
}

ngrammledt(test,name = "fivegram", ngram_count = 5)

#Function to create ngram MLE
ngrammodel<- function(tokens, name="bigram", ngram_count=2, smoothvalue=1, v=setvalue){
        setvalue<- length(unlist(tokens))
        ngrammledt(tokens, name, ngram_count, smoothvalue, v)
}
# ngrammodel(test_tokens, name="fourgram", ngram_count = 4)

#Look up tables for Shiny

ngramlookup<- function(ngrammledt){
        dt<- data.table(ngrammledt)
        dt<- unique(dt[,.(mle), by = .(frontwords, follow)])
        dt[order(-mle), .(follow, mle), by = frontwords
           ][order(-mle), .SD[1:10], by= frontwords]
}
# creating tables of each ngrams
system.time(bigram_lookup <- ngramlookup(ngrammledt(train_tokens,2,1)))
system.time(trigram_lookup <- ngramlookup(ngrammledt(train_tokens,3,1)))
system.time(fourgram_lookup <- ngramlookup(ngrammledt(train_tokens,4,1)))
system.time(fivegram_lookup <- ngramlookup(ngrammledt(train_tokens,5,1)))
# saving table data to reduce processing duration required. 
saveRDS(bigram_lookup, file = "bigram_lookup.rds")
saveRDS(trigram_lookup, file = "trigram_lookup.rds")
saveRDS(fourgram_lookup, file = "fourgram_lookup.rds")
saveRDS(fivegram_lookup, file = "fivegram_lookup.rds")

#Evaluation of ngram models on Test Set (Perplexity and Accuracy)
#Examining the time taken to process test set
print("Time taken: ")
system.time(cleaning(corp))

#Function to conduct test on test set. 
modeltest<- function(test, name = "bigram", ngram_count=2, v){
        v<- length(unique(unlist(test)))
        ngrammledt(test,name,ngram_count,v)
        
        file<- paste0(name,"_mle.rds")
        print(paste("Reading",file))
        mod<- readRDS(file)
        
        tokens<- tokensconvert(test)
        ngram_dt<- ngramsconvert(tokens, ngram_count)
        str(ngram_dt)

        splitdt<- splitgrams(ngram_dt, ngram_count)
        str(splitdt)
        mod[, c("frontwords", "follow", "mle")]
        #Applying test set
        dt<- data.table(left_join(splitdt,
                                  mod[, c("frontwords", "follow", "mle")]))
        head(dt)
        tail(dt)

        print(sum(log(1/dt[,mle]), na.rm=T)^1/v)
}
#Generate for all ngrams
testtwo<- modeltest(test,name="bigram", ngram_count=2)
testthree<- modeltest(test,name="threegram", ngram_count=3)
testfour<- modeltest(test,name="fourgram", ngram_count=4)
testfive<- modeltest(test,name="fivegram", ngram_count=5)
        
#Storing all model results
testresults<- data.frame(model = c("Bigram", "Trigram", "Fourgram", "Fivegram"), perplexity = c(testtwo,testthree,testfour,testfive))
testresults





