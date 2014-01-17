setwd("/media/docdoak/Media/Dropbox/Programming/dataAnalysis/")
load("samsungData.rda")
d <- samsungData
dim(d) 
# 7352x563
unique(d$subject)
# [1]  1  3  5  6  7  8 11 14 15 16 17 19 21 22 23 25 26 27 28 29 30
names(d)[1:15]
# [1] "tBodyAcc-mean()-X" "tBodyAcc-mean()-Y" "tBodyAcc-mean()-Z"
# [4] "tBodyAcc-std()-X"  "tBodyAcc-std()-Y"  "tBodyAcc-std()-Z" 
# [7] "tBodyAcc-mad()-X"  "tBodyAcc-mad()-Y"  "tBodyAcc-mad()-Z" 
# [10] "tBodyAcc-max()-X"  "tBodyAcc-max()-Y"  "tBodyAcc-max()-Z" 
# [13] "tBodyAcc-min()-X"  "tBodyAcc-min()-Y"  "tBodyAcc-min()-Z" 
svd1 <- svd(d[,1:15])
which.max(svd1$v[,2])
# [1] 12
try = 3
max.contrib <- which.max(svd1$v[,2])
distance.matrix <- dist(d[d$subject==try, c(10:12, max.contrib)])
hclustering <- hclust(distance.matrix)
# mypclust(hclustering, lab.col="Numeric Activity")
kClust <- kmeans(d[d$subject==try, -c(562, 563)], centers=6, nstart=100)
table(kClust$cluster, d$activity[d$subject==1])
plot(kClust$center[1,1:10], pch=19, ylab="Cluster Center", xlab="")
