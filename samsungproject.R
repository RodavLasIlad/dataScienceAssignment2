setwd("/media/docdoak/Media/Dropbox/Programming/dataAnalysis/")
load("samsungData.rda")
oldnames <- names(samsungData) # for the write up
samsungData <- data.frame(samsungData)
samsungData$activity <- as.factor(samsungData$activity)
# indexes <- sample(1:nrow(samsungData), size=0.2*nrow(samsungData))
sd.train <- subset(samsungData, samsungData$subject == c(1,3,5,6))
# train <- samsungData[-indexes,]
sd.test <- subset(samsungData, samsungData$subject == c(27,28,29,30))
# test <- samsungData[indexes,]
# train.no.s <- subset(train, select=-c(subject))
sd.train.no.s <- subset(sd.train, select=-c(subject))
sd.test.no.s <- subset(sd.test, select=-c(subject))
# test.no.s <- subset(test, select=-c(subject))
library(tree)
# trained.tree <- tree(train.no.s)
trained.tree2 <- tree(sd.train.no.s)
#summary(trained.tree)
#plot(trained.tree)
#text(trained.tree)
summary(trained.tree2)
plot(trained.tree2)
text(trained.tree2)
prunedTree <- prune.tree(trained.tree2, best=7)
plot(prunedTree)

tree1 <- tree(activity ~ ., data=sd.test.no.s)
ans <- predict(tree1, sd.test.no.s)
colnames(matrix)[apply(matrix,1,which.max)]
------------------------
setwd("/media/docdoak/Media/Dropbox/Programming/dataAnalysis/")
load("samsungData.rda")
oldnames <- names(samsungData) # for the write up
samsungData <- data.frame(samsungData)
samsungData$activity <- as.factor(samsungData$activity)
sd.train <- subset(samsungData, samsungData$subject == c(1,3,5,6))
#indexes <- sample(1:nrow(samsungData), size=0.2*nrow(samsungData))
#sd.train <- samsungData[-indexes,]
#sd.test <- samsungData[indexes,]
sd.test <- subset(samsungData, samsungData$subject == c(27,28,29,30))
sd.train.no.subj <- subset(sd.train, select=-c(subject))
sd.test.no.subj <- subset(sd.test, select=-c(subject))
library(tree)
trained.tree <- tree(activity ~ ., data=sd.train.no.subj)
summary(trained.tree)
plot(trained.tree)
text(trained.tree)
ans <- predict(trained.tree, sd.test.no.subj)
columns <- colnames(ans)[apply(ans,1,which.max)]
# table(columns)
a <- data.frame(which(columns == sd.test.no.subj))
#Percentage of correctly trained variables:
dim(a)[1] / dim(sd.test.no.subj)[1]
