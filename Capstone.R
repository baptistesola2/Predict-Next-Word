library(tools)
library(tm)
source("Coursera/Capstone/final/en_US/CapstoneFunctions.R")


# Read blog file
con <- file("Coursera/Capstone/final/en_US/en_US.blogs.txt", "r")
allLines <- readLines(con, encoding = "UTF-8", skipNul = TRUE)
close(con)


# Sample lines into test and training set
set.seed(220)
preSample <- sample(allLines, size=50000)
train <- sample(1:50000, size = 25000)
cin <- 1:50000
test <- cin[!(1:50000 %in% train)]

trainSample <- preSample[train]
testSample <- preSample[test]

# process chunk to build nGram Tables
system.time(trainModelBase <- processBlogChunk(trainSample))
system.time(testModelBase <- processBlogChunk(testSample[1000:1100]))

# 
# Create and evaluate different predictions
system.time(mulDiffCoeff_95_95_95 <- sapply(testModelBase2[[1]]$first3, FUN = function(x) predNextWord(x, simCoeff =  c(0.95,0.95,0.95), limit = c(10000,10000,10000))))
system.time(mulDiffCoeff_95_95_95_biglim <- sapply(testModelBase2[[1]]$first3, FUN = function(x) predNextWord(x, simCoeff =  c(0.95,0.95,0.95), limit = c(20000,20000,20000))))
sapply(list(mulDiffCoeff_95_95_95,mulDiffCoeff_95_95_95_biglim), evaluatePrevMulti, testModelBase2[[1]])

system.time(mulDiff95_125 <- sapply(testModelBase[[1]]$first3, FUN = function(x) predNextWord(x, simCoeff =  c(0.95,0.95,0.95), limit = c(10000,20000,50000))))
#23.9%
system.time(mulDiff95_shiny <- sapply(testModelBase[[1]]$first3, FUN = function(x) predNextWord(x, simCoeff =  c(0.95,0.95,0.95), limit = c(2000,5000,10000))))


# Create and Export image to be used by Shuny App
trainModelBase <- reduceTrainModel(trainModelBase)
rm(list = grep("trainModelBase", ls(), value = TRUE, invert = TRUE))
save.image("~/Coursera/Capstone/Captone/trainModel.RData")
# 

