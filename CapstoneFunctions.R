library(dplyr)
library(tm)
library(scales)
library(tools)

reduceTrainModel <- function(model, limit = c(10000,20000,50000)){
    for (i in 1:3){
        model[[i]] <- model[[i]][1:min(limit[i],dim(model[[i]])[1]),]
        model[[i]]$lastWord <- sapply(model[[i]]$text, FUN= function(x) strsplit(x," ")[[1]][5-i])
        model[[i]]$text <- NULL
        model[[i]]$part <- NULL
    }
    model[[4]] <- model[[4]][1:3,]
    model
}

dropTrainingModel <- function(model, limit = c(10000,20000,50000)){
    for (i in 1:3){
        model[[i]] <- model[[i]][1:min(limit[i],dim(model[[i]])[1]),]
    }
    model
}

processBlogChunk <- function(allBlogLines, sampleLines){
    sample <- allBlogLines[sampleLines]
    sample <- removeProfanity(sample)
    
    sentences <- lineToSentences(sample)
    words <- lineToWords(sample)
    n2grams <- unlist(sapply(sentences,sentToNgram,2), recursive = FALSE)
    n3grams <- unlist(sapply(sentences,sentToNgram,3), recursive = FALSE)
    n4grams <- unlist(sapply(sentences,sentToNgram,4), recursive = FALSE)
    
    n1Table <- TextFreq(words)
    n2Table <- TextFreq(n2grams)
    n3Table <- TextFreq(n3grams)
    n4Table <- TextFreq(n4grams)
    
    
    n4Table$first3 <- sapply(n4Table$text, firstGram,4)
    n3Table$first2 <- sapply(n3Table$text, firstGram,3)
    n2Table$first1 <- sapply(n2Table$text, firstGram,2)
    
    n4Table$lastWord <- sapply(n4Table$text, FUN = function(x) strsplit(x, " ")[[1]][4])
    n3Table$lastWord <- sapply(n3Table$text, FUN = function(x) strsplit(x, " ")[[1]][3])
    n2Table$lastWord <- sapply(n2Table$text, FUN = function(x) strsplit(x, " ")[[1]][2])
    
    list(n4Table,n3Table,n2Table,n1Table)
}


removeProfanity <- function(textToFilter){
    # Get profanity from external source 
    profanity <- readLines("Coursera/Capstone/final/en_US/enProfanity.txt")
    profanityTitle <- toTitleCase(profanity)
    profanityUpper <- toupper(profanity)
    
    # Suppress profanity words from Sample of lines
    profanity <- append(profanity,profanityTitle)
    profanity <- append(profanity,profanityUpper)
    textToFilter <- removeWords(textToFilter,words = profanity)
    textToFilter
}


cleanToSentences <- function(text) {
    textClean <- removeWords(text,words = profanity)
    textSent <- strsplit(textClean,"\\.|\\!|\\?|\\|,|;|:")
    tmp <- vector()
    len <- length(textSent)
    for (i in 1:len) {tmp <- append(tmp,as.character(textSent[[i]]))}
    tmp[nchar(tmp)>2]
}

cleanToWords <- function(text) {
    textClean <- removeWords(text,words = profanity)
    textWord <- strsplit(textClean,"\\ |,|;|:|\\/|\\*|\\.|\\!|\\?|\\|")
    tmp <- vector()
    len <- length(textWord)
    for (i in 1:len) {tmp <- append(tmp,as.character(textWord[[i]]))}
    tmp[tmp != ""]
}

punctuationInSentence <- c(",",";",":")

# Get foreign langages by measuring the occurnce of words in the pairs (if a word is rare and is never paired with a more frequent word then it more likely a foreigh word)
# Get foreign language from sentence without most common words
# Get foreign word by looking for roots 
# Improve speed of sentHasWord

TextFreq <- function(text){
    text <- tolower(text)
    t <- as.data.frame(table(text))
    t$text <- as.character(t$text)
    t<- arrange(t,desc(nchar(text)))
    t<- arrange(t,desc(Freq))
    t
}
isSameRoot <- function(wordToTest, wordRef, minLength=4){
    exact <- wordToTest == wordRef
    match80 <- FALSE
    nLetters <- nchar(wordToTest)
    for (i in 1:abs(nLetters*0.2)) {
        match80 <- match80 | grepl(substr(x = wordToTest, start = i, stop = i+abs(nLetters*0.8)),wordRef, fixed = TRUE)
    }
    exact|(match80&nLetters>=minLength)
}

wordPairing <- function(textVector){
    s <- vector()
    textClean <- textVector[textVector!=""]
    for (i in 1:(length(textClean)-1)){
        dd <- paste(textClean[i], textClean[i+1])
        s <- append(s,paste(textClean[i], textClean[i+1]))
    }
    s
}

word3ing <- function(textVector){
    s <- vector()
    textClean <- textVector[textVector!=""]
    for (i in 1:(length(textClean)-2)){
        s <- append(s,paste(textClean[i], textClean[i+1], textClean[i+2]))
    }
    s
}

word4ing <- function(textVector){
    s <- vector()
    textClean <- textVector[textVector!=""]
    for (i in 1:(length(textClean)-3)){
        s <- append(s,paste(textClean[i], textClean[i+1], textClean[i+2], textClean[i+3]))
    }
    s
}

#Takes a sentence as input 
sentToNgram <- function(sentence, n){
    words <- lineToWords(sentence)
    words <- words[!words==""]
    nGrams <- vector()
    if (length(words)>=n){
        for (i in 1:(length(words)-n+1)){
            tmp <- character(0)
            for(j in 1:n){
                if (j == 1)
                    tmp <- words[i+j-1]
                else
                    tmp <- paste(tmp,words[i+j-1])
            }
            nGrams <- append(nGrams,tmp)
        }
    }
    nGrams
}

WordPairs <- function(rawText) {
    textClean <- removeWords(rawText,words = profanity)
    textClean <- removeWords(textClean, words = punctuationInSentence)
    textSent <- strsplit(textClean,"\\.|\\!|\\?|\\|")
    ss <- lapply(textSent,cleanToWords)
    textPairs <- lapply(ss, wordPairing)
    tmp <- vector()
    len <- length(textPairs)
    for (i in 1:len) {tmp <- append(tmp,textPairs[[i]])}
    tmp
}

Word3grams <- function(rawText) {
    textClean <- removeWords(rawText,words = profanity)
    textClean <- removeWords(textClean, words = punctuationInSentence)
    textSent <- strsplit(textClean,"\\.|\\!|\\?|\\|")
    ss <- lapply(textSent,cleanToWords)
    textPairs <- lapply(ss, word3ing)
    tmp <- vector()
    len <- length(textSent)
    for (i in 1:len) {tmp <- append(tmp,textPairs[[i]])}
    tmp
}

Word4grams <- function(rawText) {
    textClean <- removeWords(rawText,words = profanity)
    textClean <- removeWords(textClean, words = punctuationInSentence)
    textSent <- strsplit(textClean,"\\.|\\!|\\?|\\|")
    ss <- lapply(textSent,cleanToWords)
    textPairs <- lapply(ss, word4ing)
    tmp <- vector()
    len <- length(textSent)
    for (i in 1:len) {tmp <- append(tmp,textPairs[[i]])}
    tmp
}

lineToSentences <- function(text) {
    textSent <- strsplit(text,"\\.|\\!|\\?|\\||,|;|:")
    tmp <- vector()
    for (i in 1:length(textSent)) {tmp <- append(tmp,as.character(textSent[[i]]))}
    # we filter out the sentence with less than 2 characters
    tmp[nchar(tmp)>2]
}

lineToWords <- function(text) {
    textWord <- strsplit(text,"\\ |,|;|:|\\/|\\*|\\.|\\!|\\?|\\||\\(|\\)")
    tmp <- vector()
    for (i in 1:length(textWord)) {tmp <- append(tmp,removeNumbers(as.character(textWord[[i]])))}
    # We filter out the empty characters or non letter cha
    removePunctuation(tmp)[removePunctuation(tmp) != ""]
}


realSentence <- function(sentences){
    nbChar <- nchar(sentences)
    nbWords <- sapply(strsplit(sentences,"\\ |,|;|:|\\/|\\*|\\.|\\!|\\?|\\|"),length)
    isNumber <- !grepl("[[:alpha:]]", head(sentence10000))
    sentences[(!isNumber)&(nbChar>5)&nbWords>1]
}

wordInTopWord <- function(wordToTest, topWords) {
    hasTopWord <- FALSE
    nbWords <- length(topWords)
    for (i in 1:nbWords){
        if (tolower(wordToTest) == tolower(topWords[i])){
            return(TRUE)}
    }
    hasTopWord
}

sentHasTopWords <- function(sent, topWords){
    # cut sentences to word to avoid matching only letters (only exact word match)
    # print(sent)
    if (!grepl("[[:alpha:]]", sent)){FALSE}
    else {
        wordInSent <- lapply(sent,cleanToWords)
        wordTested <- sapply(wordInSent,wordInTopWord, topWords)
        length(wordTested[wordTested]) >0
    }
}



#Takes a sentence as input 
sentToNgram <- function(sentence, n){
    words <- lineToWords(sentence)
    nGrams <- vector()
    if (length(words)>=n){
        for (i in 1:(length(words)-n+1)){
            tmp <- character(0)
            for(j in 1:n){
                if (j == 1)
                    tmp <- words[i+j-1]
                else
                    tmp <- paste(tmp,words[i+j-1])
            }
            nGrams <- append(nGrams,tmp)
        }
    }
    nGrams
}

firstGram <- function(nGram,n){
    tmp <- strsplit(nGram," ")
    v <- ""
    for (i in 1:n-1)
        if(i == 1 )
            v <- tmp[[1]][i]
        else
            v <- paste(v, tmp[[1]][i])
    v
}



