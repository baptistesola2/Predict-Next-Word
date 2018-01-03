# Is there a better res when there is multiple matches?
library(RecordLinkage)
library(tm)
library(scales)
library(dplyr)
library(plyr)

#Model non recuresif pour exploration

pWord3 <- function(n3gram, n4Tableu = n4Table, simCoeff =0.9){
    closePred <- vector()
    closePredIndex <- ClosestMatch2(tolower(n3gram), n4Tableu$first3, simCoeff)
    if (length(closePredIndex)>0)
        closePred <-n4Tableu$lastWord[closePredIndex]
    closePred
}

pWord2 <- function(n2gram, n3Tableu = n3Table, simCoeff =0.9){
    closePred <- vector()
    closePredIndex <- ClosestMatch2(tolower(n2gram), n3Tableu$first2, simCoeff)
    if (length(closePredIndex)>0)
        closePred <-n3Tableu$lastWord[closePredIndex]
    closePred
    
}
pWord1 <- function(n1gram, n2Tableu , simCoeff =0.9){
    closePred <- vector()
    closePredIndex <- ClosestMatch2(tolower(n1gram), n2Tableu$first1, simCoeff)
    if (length(closePredIndex)>0)
        closePred <-n2Tableu$lastWord[closePredIndex]
    closePred
}
predNextWord <- function(nGram, n4Tableu = trainModelBase[[1]], n3Tableu = trainModelBase[[2]]
                         , n2Tableu = trainModelBase[[3]], n1Tableu= trainModelBase[[4]], simCoeff = c(0.9, 0.9,0.9) ) {
    tmp <- lineToWords(nGram)
    nWords <- length(tmp)
    p3 <- vector()
    p2 <- vector()
    p1 <- vector()
    p0 <- vector()
    if (nWords>=3)
        p3 <- unique(pWord3(paste(tmp[nWords-2],tmp[nWords-1], tmp[nWords]),n4Tableu, simCoeff[1]))
    if (nWords>=2 & length(p3)<3)
        p2 <- unique(pWord2(paste(tmp[nWords-1], tmp[nWords]), n3Tableu, simCoeff[2]))
    if (nWords>=1& length(p3)+length(p2)<3)
        p1 <- unique(pWord1(tmp[nWords], n2Tableu, simCoeff[3]))
    p0 <- n1Tableu$text[1:3]
    # append(p3,append(p2,append(p1,p0)))[1:3]
    tmp <- append(p3,append(p2,append(p1,p0)))
    unique(tmp)[1:3]
}



ClosestMatch2 = function(string, stringVector,limitSim=0.9){
    # test pour accélérer
    # sizes <- sapply(stringVector, nchar)
    # tmp <- nchar(string)
    # stringVector[sizes<tmp*limitSim | sizes > tmp/limitSim] <- ""
    distance = levenshteinSim(as.character(string), as.character(stringVector));
    out <- 1:length(stringVector)
    out <- out[distance>limitSim]
    distance = distance[distance>limitSim]
    if (length(out)>0){
        out[order(-distance)]
    }
    else
        return(vector())
}

nGramNew <- function(nGramPasted, modelTable){
    ngram <- lineToWords(tolower(nGramPasted))
    n <- length(ngram)
    first <- ngram[1]
    last <- ngram[n]
    if(n>=3)
        first <- paste(first,ngram[2])
    if(n>=4)
        first <- paste(first,ngram[3])
    mat <- match(paste(first,last), paste(modelTable$first,modelTable$lastWord))
    if(is.na(mat)){
        newEntry <- c(modelTable$Freq[1], first, ngram[n])
        modelTable <- rbind(newEntry,modelTable)
        modelTable$Freq <- as.numeric(modelTable$Freq)
    }
    else{
        modelTable$Freq[mat] = as.numeric(modelTable$Freq[mat])+1
    }
    modelTable
}

learnNewNgram <- function(textTolearn,model){
    words <- lineToWords(textTolearn)
    nWords <- length(words)
    if(nWords>= 4){
        gram4 <- sentToNgram(textTolearn,4)
        for(j in 1:length(gram4))
            model[[1]] = nGramNew(gram4[j],model[[1]])
    }
    if(nWords>= 3){
        gram3 <- sentToNgram(textTolearn,3)
        for(j in 1:length(gram3))
            model[[2]] = nGramNew(gram3[j],model[[2]])
    }
    if(nWords>= 2){
        gram2 <- sentToNgram(textTolearn,2)
        for(j in 1:length(gram2))
            model[[3]] = nGramNew(gram2[j],model[[3]])
    }
    model
}

lineToWords <- function(text) {
    textWord <- strsplit(text,"\\ |,|;|:|\\/|\\*|\\.|\\!|\\?|\\||\\(|\\)")
    tmp <- vector()
    for (i in 1:length(textWord)) {tmp <- append(tmp,removeNumbers(as.character(textWord[[i]])))}
    # We filter out the empty characters or non letter cha
    removePunctuation(tmp)[removePunctuation(tmp) != ""]
}

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
