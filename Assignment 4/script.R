Complex9_RN32 <- read.csv("~/Desktop/DataMining/Assignment 4/Complex9_RN32.txt",header = FALSE) #Romita, change working directory.

#Task 0 
#Plot the Complex9_RN32 dataset. 
library(stats)
addColor<-c("red","orange","yellow","green","green4","blue","purple","cyan","pink")
plot(Complex9_RN32$V2~Complex9_RN32$V1,col=addColor[Complex9_RN32$V3+1])

#Task 2
#First Technique: kNN function with manhattan distance. 
averageDistanceOutlier <-function(XYData){
  numberOfClosestNeighbors <- 15
  dataset<-data.frame(x=XYData[,1],y=XYData[,2],c=XYData[,3],ols=0)
  datasetDistance<-as.matrix(dist(dataset[,1:2], method= "manhattan"))
  rows<-nrow(datasetDistance)
  for(i in 1:rows){
    list<-datasetDistance[i,]
    orderedList<-order(list,decreasing= FALSE)
    average<-0
    for(j in 2:(numberOfClosestNeighbors+1)){
      average<-average+datasetDistance[i,orderedList[j]]
    }
    average<-average/numberOfClosestNeighbors
    dataset[i,]$ols <-average
  }
  return(dataset)
}

#Task 3
#Calculate the outlier score 'ols' and write to file named X.
averageDistanceOutlierResult<-averageDistanceOutlier(Complex9_RN32)
write.csv(averageDistanceOutlierResult,"X.csv")

#Order the outlier score descending in dataset. 
ol<-order(averageDistanceOutlierResult$ols,decreasing = T)
outlierScoreDescending<-averageDistanceOutlierResult[ol,]

#Vizualize the first 9% and 91%
outlierObservation = outlierScoreDescending[1:360,]
plot(outlierObservation$y~outlierObservation$x,col=addColor[outlierObservation$c+1])

normalObservation = outlierScoreDescending[361:3999,]
plot(normalObservation$y~normalObservation$x,col=addColor[normalObservation$c+1])

#Vizualize the first 18% and 82%
outlierObservation = outlierScoreDescending[1:720,]
plot(outlierObservation$y~outlierObservation$x,col=addColor[outlierObservation$c+1])

normalObservation = outlierScoreDescending[721:3999,]
plot(normalObservation$y~normalObservation$x,col=addColor[normalObservation$c+1])

#Vizualize the first 36% and 64%
outlierObservation= outlierScoreDescending[1:1440,]
plot(outlierObservation$y~outlierObservation$x,col=addColor[outlierObservation$c+1])

normalObservation = outlierScoreDescending[1441:3999,]
plot(normalObservation$y~normalObservation$x,col=addColor[normalObservation$c+1])

#histogram for 36% outlier score.
hist(outlierObservation$ols)
############################################################################################################
#Second Technique: EM Clustering 
install.packages("mclust")
setwd("~/Desktop/DataMining/Assignment 4/") #Romita, change working directory. 

Complex9_RN32 <- read.table("Complex9_RN32.txt", header = FALSE, sep = ",")
normalized <- scale(Complex9_RN32[ ,1:2])
df <- data.frame(normalized)
df$V3 = Complex9_RN32[ ,3]

#Task 0
plot(Complex9_RN32$V1, Complex9_RN32$V2, col = Complex9_RN32$V3)

#Task 2
library(mclust)
mclust <- Mclust(normalized[ ,1:2])
plot(mclust)
density <- densityMclust(normalized[ ,1:2])
ols <- predict(density, what = "dens", logarithm = TRUE)
stat_df <- data.frame(Complex9_RN32, ols)
sorted <- stat_df[order(stat_df$ols), ]

#Task 3
display_9 <- sorted[1:(.09*nrow(sorted)), ]
display_91 <- sorted[(.09*nrow(sorted)):nrow(sorted), ]
display_18 <- sorted[1:(.18*nrow(sorted)), ]
display_82 <- sorted[(.18*nrow(sorted)):nrow(sorted), ]
display_36 <- sorted[1:(.36*nrow(sorted)), ]
display_64 <- sorted[(.36*nrow(sorted)):nrow(sorted), ]

addColor <- c("red","orange","yellow","green","green4","blue","purple","cyan","pink")
plot(display_9$V1, display_9$V2, col = addColor[display_9$V3+1])
plot(display_91$V1, display_91$V2 ,col = addColor[display_91$V3+1])
plot(display_18$V1, display_18$V2, col = addColor[display_18$V3+1])
plot(display_82$V1, display_82$V2 ,col = addColor[display_82$V3+1])
plot(display_36$V1, display_36$V2, col = addColor[display_36$V3+1])
plot(display_64$V1, display_64$V2 ,col = addColor[display_64$V3+1])

hist(display_36$ols)